#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <cuda_runtime.h>
#include <cublas.h>
#include <cublas_v2.h>


#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/signals.h>

#define check_status(status) \
  if (status != CUBLAS_STATUS_SUCCESS) { \
    fprintf(stderr, "ERREUR %d\n", __LINE__);     \
    exit(1); \
  }

struct cublas_vect {
  float* f;
  int len;
};

struct cublas_mat {
  float* f;
  int dim1, dim2;
};

cublasHandle_t handle;

float* value_to_floatstar(value v){
  struct cublas_vect * cv = (struct cublas_vect *)Data_custom_val(v);
  return cv->f;
}
int value_len(value v){
  struct cublas_vect * cv = (struct cublas_vect *)Data_custom_val(v);
  return cv->len;
}

float* value_to_floatstar_mat(value v){
  struct cublas_mat * cv = (struct cublas_mat *)Data_custom_val(v);
  return cv->f;
}
int value_dim1(value v){
  struct cublas_mat * cv = (struct cublas_mat *)Data_custom_val(v);
  return cv->dim1;
}
int value_dim2(value v){
  struct cublas_mat * cv = (struct cublas_mat *)Data_custom_val(v);
  return cv->dim2;
}

static void finalize_cublas_vect(value t ){
  float* ptr = value_to_floatstar(t);
  cublasFree(ptr);
}
static void finalize_cublas_mat(value t ){
  float* ptr = value_to_floatstar_mat(t);
  cublasFree(ptr);
}

static struct custom_operations cublas_vect__ops =
{
  "ocaml_cublas_vector",
  finalize_cublas_vect,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static struct custom_operations cublas_mat__ops =
{
  "ocaml_cublas_matrix",
  finalize_cublas_mat,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value floatstar_to_value(float* ptr, int len){
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_custom(&cublas_vect__ops, sizeof(struct cublas_vect),1000, 1000000);
  ((struct cublas_vect *)Data_custom_val(v))->len = len;
  ((struct cublas_vect *)Data_custom_val(v))->f = ptr;
  CAMLreturn(v);
}

value floatstar_mat_to_value(float* ptr, int dim1, int dim2){
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_custom(&cublas_mat__ops, sizeof(struct cublas_mat),1000, 1000000);
  ((struct cublas_mat *)Data_custom_val(v))->dim1 = dim1;
  ((struct cublas_mat *)Data_custom_val(v))->dim2 = dim2;
  ((struct cublas_mat *)Data_custom_val(v))->f = ptr;
  CAMLreturn(v);
}

CAMLprim value cublas_scale(value vect, value caml_alpha){
  CAMLparam1 (vect);
  float* ptr = value_to_floatstar(vect);
  mlsize_t len = value_len(vect);
  float alpha = Double_val(caml_alpha);
  check_status(cublasSscal(handle, len, &alpha, ptr, 1));
  CAMLreturn(Val_unit);
}

CAMLprim value cublas_vect_of_array(value caml_array){
  CAMLparam1 (caml_array);
  float* ptr;
  mlsize_t len = caml_array_length(caml_array);
  check_status(cublasAlloc( len, sizeof(float), (void**)&ptr));
  float *cdata = malloc (len * sizeof(float));
  int i;
  for (i = 0; i < len; ++i)
    cdata[i] = Double_field(caml_array, i);
  check_status(cublasSetVector(len, sizeof(float), cdata, 1, ptr, 1));
  free(cdata);
  CAMLreturn(floatstar_to_value(ptr, len));
}
CAMLprim value cublas_matrix_of_array(value transposed, value caml_array){
  CAMLparam2 (transposed, caml_array);
  CAMLlocal1(tmp_tab);
  int transposed_ = Val_bool(transposed);
  float* ptr;
  mlsize_t dim1 = caml_array_length(caml_array);
  mlsize_t dim2 = caml_array_length(Field(caml_array, 0));
  int len = dim1 * dim2;
  check_status(cublasAlloc( len, sizeof(float), (void**)&ptr));
  float *cdata = malloc (len * sizeof(float));
  int i, j;
  for (i = 0; i < dim1; ++i){
    tmp_tab = Field(caml_array, i);
    for (j = 0; j < dim2; ++j){
      if (transposed_){
        cdata[i + j * dim1] = Double_field(tmp_tab, j);
      }else{
        cdata[i * dim2 + j] = Double_field(tmp_tab, j);
      }
    }
  }
  check_status(cublasSetVector(len, sizeof(float), cdata, 1, ptr, 1));
  free(cdata);
  CAMLreturn(floatstar_mat_to_value(ptr, dim1, dim2));
}

CAMLprim value cublas_vect_copy(value vect0){
  CAMLparam1 (vect0);
  float* ptr;
  int len = value_len(vect0);
  check_status(cublasAlloc( len, sizeof(float), (void**)&ptr));  
  check_status(cublasScopy(handle, len, value_to_floatstar(vect0), 1, ptr, 1));
  CAMLreturn(floatstar_to_value(ptr, len));
}
CAMLprim value cublas_matrix_copy(value vect0){
  CAMLparam1 (vect0);
  float* ptr;
  int dim1 = value_dim1(vect0), dim2 = value_dim2(vect0);
  int len = dim1 * dim2;
  check_status(cublasAlloc( len, sizeof(float), (void**)&ptr));  
  check_status(cublasScopy(handle, len, value_to_floatstar(vect0), 1, ptr, 1));
  CAMLreturn(floatstar_mat_to_value(ptr, dim1, dim2));
}
cublasOperation_t int2trans(int i){
  i = i & 3;
  if (i == 1) return CUBLAS_OP_T;
  if (i == 2) return CUBLAS_OP_C;
  return CUBLAS_OP_N;
}


CAMLprim value cublas_mul_matrix(value v1, value v2, value alpha, value trans){
  CAMLparam4 (v1, v2, alpha, trans);

  int trans_i = Int_val(trans);
  cublasOperation_t ta = int2trans(trans_i);
  cublasOperation_t tb = int2trans(trans_i << 3);

  int dim1 = value_dim1(v1), dim2 = value_dim2(v1);
  int dim3 = value_dim1(v2), dim4 = value_dim2(v2);
  
  float alpha_f = Double_val(alpha);
  float beta_f = 0;

  float* ptr;

  int m = ta == CUBLAS_OP_T ? dim2 : dim1;
  int n = tb == CUBLAS_OP_T ? dim3 : dim4;
  int k = tb == CUBLAS_OP_T ? dim4 : dim3;
  
  int len = m * n;
  check_status(cublasAlloc( len, sizeof(float), (void**)&ptr)); 
  check_status(cublasSgemm(handle, ta, tb,
                           m, n, k,
                           &alpha_f,
                           value_to_floatstar_mat(v1), m,
                           value_to_floatstar_mat(v2), k,
                           &beta_f,
                           ptr, m
                           ));

  CAMLreturn(floatstar_mat_to_value(ptr, m, n));
}
CAMLprim value cublas_mul_matrix_vector(value v1, value v2, value alpha, value beta, value trans){
  CAMLparam5 (v1, v2, alpha, beta, trans);
  int dim1 = value_dim1(v1), dim2 = value_dim2(v1);
  float* ptr;
  float alpha_f = Double_val(alpha);
  float beta_f = Double_val(beta);
  int trans_i = Int_val(trans);
  cublasOperation_t t = int2trans(trans_i);

  int m = t == CUBLAS_OP_T ? dim2 : dim1;
  int n = t == CUBLAS_OP_T ? dim1 : dim2;
  int len = t == CUBLAS_OP_N ? dim1 : dim2;
  
  check_status(cublasAlloc( len, sizeof(float), (void**)&ptr));
  check_status(cublasSgemv(handle, t,
                           dim1, dim2,
                           &alpha_f,
                           value_to_floatstar_mat(v1),
                           dim1,
                           value_to_floatstar(v2),
                           1,
                           &beta_f,
                           ptr, 1
                           ));

  CAMLreturn(floatstar_to_value(ptr, len));
}

CAMLprim value cublas_mul(value v1, value v2){
  CAMLparam2 (v1, v2);
  int len = value_len(v1);
  float* ptr;
  check_status(cublasAlloc( len, sizeof(float), (void**)&ptr));
  check_status(cublasSdgmm( handle, CUBLAS_SIDE_LEFT,
              len, 1,
              value_to_floatstar(v1), len,
              value_to_floatstar(v2) , 1,
              ptr, len));
  /*
check_status(cublasDotEx (handle, len,
             value_to_floatstar(v1), CUDA_R_32F, 1,
             value_to_floatstar(v2), CUDA_R_32F, 1,
             ptr, CUDA_R_32F, CUDA_R_32F)); ça calcule la somme. */

  CAMLreturn(floatstar_to_value(ptr, len));
}

CAMLprim value cublas_ssqr(value vect){
  CAMLparam1(vect);
  CAMLlocal1(ml_f);
  float result;
  float* x = value_to_floatstar(vect);
  check_status(cublasDotEx (handle, value_len(vect), x, CUDA_R_32F, 1, x, CUDA_R_32F, 1, &result, CUDA_R_32F, CUDA_R_32F));
  ml_f = caml_copy_double(result);
  CAMLreturn(ml_f);
}

CAMLprim value cublas_vect_incr(value v1, value v2, value alpha){
  CAMLparam3 (v1, v2, alpha);
  int len = value_len(v1);
  float alpha_f = Double_val(alpha);
  check_status(cublasSaxpy( handle, len,
                            &alpha_f,
                            value_to_floatstar(v1), 1, value_to_floatstar(v2), 1));
  CAMLreturn(Val_unit);
}
CAMLprim value cublas_matrix_incr(value v1, value v2, value alpha){
  CAMLparam3 (v1, v2, alpha);
  int len = value_dim1(v1) * value_dim2(v1);
  float alpha_f = Double_val(alpha);
  check_status(cublasSaxpy( handle, len,
                            &alpha_f,
                            value_to_floatstar_mat(v1), 1, value_to_floatstar_mat(v2), 1));
  CAMLreturn(Val_unit);
}

value array_of_ptr(float* ptr, int len, int incr, int free_){
  CAMLparam0();
  CAMLlocal1(caml_array);
  if (len == 0)
    CAMLreturn(Atom(0));
  caml_array = caml_alloc(len, Double_array_tag);
  int i;
  for (i = 0; i < len; ++i)
    Store_double_field(caml_array, i, ptr[i * incr]);
  if (free_) free(ptr);
  CAMLreturn( caml_array );
}

CAMLprim value cublas_array_of_vect(value vect){
  CAMLparam1 (vect);
  float* ptr = value_to_floatstar(vect);
  int len = value_len(vect);
  float *cdata = malloc (len * sizeof(float));
  check_status(cublasGetVector(len, sizeof(float), ptr, 1, cdata, 1));
  CAMLreturn( array_of_ptr(cdata, len, 1, 1));
}
CAMLprim value cublas_array_of_matrix(value transposed, value mat){
  CAMLparam2 (transposed, mat);
  CAMLlocal1(caml_array);
  int transposed_ = Val_bool(transposed);
  float* ptr = value_to_floatstar_mat(mat);
  int d1 = value_dim1(mat);
  int d2 = value_dim2(mat);
  int len = d1 * d2;
  float *cdata = malloc (len * sizeof(float));
  check_status(cublasGetVector(len, sizeof(float), ptr, 1, cdata, 1));
  caml_array = caml_alloc(d1, 0);
  int i;
  for (i = 0; i < d1; ++i){
    Store_field(caml_array, i, transposed_ ?
                array_of_ptr(cdata+ i, d2, d1, 0) :
                array_of_ptr(cdata+ i * d2, d2, 1, 0)
                );
  }
  free(cdata);
  CAMLreturn( caml_array );
}

CAMLprim value cublas_init(value unit){
  CAMLparam0();
  CAMLxparam1(unit);
  cublasInit();
  check_status(cublasCreate(&handle));
  CAMLreturn(Val_unit);
}

CAMLprim value cublas_shutdown(value unit){
  CAMLparam0();
  CAMLxparam1(unit);
  check_status(cublasDestroy(handle));
  check_status(cublasShutdown());
  CAMLreturn(Val_unit);
}
