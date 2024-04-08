load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/Image 0x40000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/linux_2c.dtb 0x40ef0000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/rootfs.cpio 0x41000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000
targets riscv.cpu.1; resume
targets riscv.cpu.0; resume
