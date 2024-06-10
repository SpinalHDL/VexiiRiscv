export WORK_DIR=$PWD

git clone https://github.com/SpinalHDL/VexiiRiscv
export VEXII=$PWD/VexiiRiscv

git clone http://github.com/buildroot/buildroot
cd buildroot
make O=build/basic  BR2_EXTERNAL=$VEXII/doc/litex/buildroot litex_soc_basic_defconfig
(cd build/basic/ && make -j$(nproc))

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv --cpu-variant=debian  --bus-standard axi-lite \
--cpu-count=1 --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma  \
--soc-json build/csr.json --build
