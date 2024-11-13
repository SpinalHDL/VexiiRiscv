set outfile1 [open "trace.out" w+]   
for {set k 0} {$k < 10000} {incr k} { 
  puts $outfile1 [irscan riscv.cpu 0x17; drscan riscv.cpu 32 0] 
} 
close $outfile1

