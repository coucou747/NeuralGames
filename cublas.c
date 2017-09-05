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

struct cublas_vect {
  float* f;
  int len;
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


static void finalize_cublas_vect(value t ){
  float* ptr = value_to_floatstar(t);
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

value floatstar_to_value(float* ptr, int len){
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_custom(&cublas_vect__ops, sizeof(float*),1000, 1000000);
  ((struct cublas_vect *)Data_custom_val(v))->len = len;
  ((struct cublas_vect *)Data_custom_val(v))->f = ptr;
  CAMLreturn(v);
}

CAMLprim value cublas_scale(value vect, value caml_alpha){
  CAMLparam1 (vect);
  float* ptr = value_to_floatstar(vect);
  mlsize_t len = value_len(vect);
  float alpha = Double_val(caml_alpha);
  cublasSscal(handle, len, &alpha, ptr, 1);
  CAMLreturn(Val_unit);
}

CAMLprim value cublas_vect_of_array(value caml_array){
  CAMLparam1 (caml_array);
  float* ptr;
  mlsize_t len = Wosize_val(caml_array);
  cublasAlloc( len, sizeof(float), (void**)&ptr);
  float *cdata = malloc (len * sizeof(float));
  int i;
  for (i = 0; i < len; ++i)
    cdata[i] = Double_field(caml_array, i);
  cublasSetVector(len, sizeof(float), cdata, 1, ptr, 1);
  CAMLreturn(floatstar_to_value(ptr, len));
}

CAMLprim value cublas_array_of_vect(value vect){
  CAMLparam1 (vect);
  CAMLlocal1(caml_array);
  float* ptr = value_to_floatstar(vect);
  mlsize_t len = value_len(vect);
  float *cdata = malloc (len * sizeof(float));
  cublasGetVector(len, sizeof(float), ptr, 1, cdata, 1);
  caml_array = caml_alloc(len, Double_array_tag);
  int i;
  for (i = 0; i < len; ++i)
    Store_double_field(caml_array, i, cdata[i]);
  CAMLreturn( caml_array );
}
CAMLprim value cublas_init(value unit){
  CAMLparam0();
  CAMLxparam1(unit);
  cublasInit();

  cublasStatus_t status = cublasCreate(&handle);
  if (status != CUBLAS_STATUS_SUCCESS) {
    fprintf(stderr, "ERREUR !\n");
  }
  
  CAMLreturn(Val_unit);
}
CAMLprim value cublas_shutdown(value unit){
  CAMLparam0();
  CAMLxparam1(unit);
  cublasDestroy(handle);
  cublasShutdown();
  CAMLreturn(Val_unit);
}
