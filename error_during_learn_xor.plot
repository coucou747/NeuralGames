
set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 1024, 1024 
set output "error_during_learn_xor.plot.png"
set style line 1 lc rgb '#0060ad' lt 1 lw 1 pt 1 ps 1.5   # --- blue

datafile='error_during_learn_xor.dat'

stats datafile

set logscale x
plot for [i=1:STATS_blocks] datafile index (i-1) with line ls 1 notitle