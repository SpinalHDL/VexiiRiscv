
load_image /media/data2/proj/vexii/VexiiRiscv/ext/NaxSoftware/baremetal/prefetch_litex/build/rv64ima/prefetch_litex.bin 0x40000000
targets riscv.cpu.0
reg pc 0x40000000
resume

targets riscv.cpu.1
reg pc 0x40000000
#resume


echo "done"
exit