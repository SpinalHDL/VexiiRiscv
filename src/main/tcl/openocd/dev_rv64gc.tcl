
set cpu_count 1
if [info exists env(HART_COUNT)]  {
    set cpu_count $::env(HART_COUNT)
}

#load_image /media/data2/proj/vexii/litex/buildroot/dts/rv64gc_spi.dtb 0x40ef0000
#load_image /media/data2/proj/vexii/litex/buildroot/buildroot/build/rv64gc/images/Image 0x40000000
#load_image /media/data2/proj/vexii/litex/buildroot/buildroot/build/rv64gc/images/rootfs.cpio 0x41000000
#load_image /media/data2/proj/vexii/litex/buildroot/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000

load_image /media/data2/proj/vexii/litex/debian/

load_image /media/data2/proj/vexii/litex/debian/Image 0x41000000
load_image /media/data2/proj/vexii/litex/debian/linux.dtb 0x46000000
load_image /media/data2/proj/vexii/litex/debian/opensbi.bin 0x40f00000


for {set i 0} {$i < $cpu_count} {incr i} {
    targets $_TARGETNAME.$i
    resume
}

