
set cpu_count 1
if [info exists env(HART_COUNT)]  {
    set cpu_count $::env(HART_COUNT)
}

reset halt

targets $_TARGETNAME.0
resume
after 1300

halt
load_image /media/data2/proj/vexii/litex/debian/linux.dtb 0x46000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000

for {set i 0} {$i < $cpu_count} {incr i} {
    targets $_TARGETNAME.$i
    reg pc 0x40f00000
    reg a0 0
    reg a1 0
    reg a2 0
    reg a3 0
    #resume
}





