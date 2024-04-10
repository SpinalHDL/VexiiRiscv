
set cpu_count 1
if [info exists env(HART_COUNT)]  {
    set cpu_count $::env(HART_COUNT)
}

load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/linux_${cpu_count}c.dtb 0x40ef0000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/Image 0x40000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/rootfs.cpio 0x41000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000

for {set i 0} {$i < $cpu_count} {incr i} {
    targets $_TARGETNAME.$i
    resume
}

