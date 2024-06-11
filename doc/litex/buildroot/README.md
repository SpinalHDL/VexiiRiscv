## Introduction

This readme document (from scratch) how to generate all the images to run buildroot Litex + VexiiRiscv.

## Setup environnement variables

```shell
export WORK_DIR= where to put projects
export TOOL= where to install a few dependencies
```


## Install dependencies

Tested on Linux Mint 21.2 

```shell
# misc
sudo apt install git make gcc device-tree-compiler ninja-build qtbase5-dev openocd
sudo pip3 install meson

# JAVA JDK
sudo add-apt-repository -y ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install openjdk-19-jdk -y # You don't exactly need that version
sudo update-alternatives --config java
sudo update-alternatives --config javac

# Install SBT - https://www.scala-sbt.org/
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt-get update
sudo apt-get install sbt

# RISC-V gcc
cd $TOOLS
wget https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack/releases/download/v12.3.0-2/xpack-riscv-none-elf-gcc-12.3.0-2-linux-x64.tar.gz
tar -xvzf xpack-riscv-none-elf-gcc-12.3.0-2-linux-x64.tar.gz
export PATH=$TOOLS/xpack-riscv-none-elf-gcc-12.3.0-2/bin:$PATH

cd $TOOLS
wget https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2024.04.12/riscv64-glibc-ubuntu-22.04-gcc-nightly-2024.04.12-nightly.tar.gz
tar -xvzf riscv64-glibc-ubuntu-22.04-gcc-nightly-2024.04.12-nightly.tar.gz
export PATH=$TOOLS/riscv/bin:$PATH

# install litex
cd $TOOLS
mkdir litex
cd litex
wget https://raw.githubusercontent.com/enjoy-digital/litex/master/litex_setup.py
chmod +x litex_setup.py
./litex_setup.py --init --install --user
```

## Build buildroot images

```shell
cd $WORK_DIR
git clone https://github.com/SpinalHDL/VexiiRiscv
git clone http://github.com/buildroot/buildroot
cd buildroot
make O=build/basic  BR2_EXTERNAL=$WORK_DIR/VexiiRiscv/doc/litex/buildroot litex_soc_basic_defconfig
(cd build/basic/ && make -j$(nproc))
```

## Generate the hardware

```shell
cd $WORK_DIR
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv --cpu-variant=linux  --bus-standard axi-lite \
--cpu-count=1 --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma  \
--soc-json build/csr.json --build
python3 -m litex.tools.litex_json2dts_linux build/csr.json > build/linux.dts
dtc -O dtb -o build/linux.dtb build/linux.dts
```

## Load the hardware

```shell
cd $WORK_DIR
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv --cpu-variant=linux  --bus-standard axi-lite \
--cpu-count=1 --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma  \
--soc-json build/csr.json --load
```

## Build opensbi

```shell
cd $WORK_DIR
git clone https://github.com/Dolu1990/opensbi.git --branch upstream
cd opensbi
make CROSS_COMPILE=riscv64-unknown-linux-gnu- \
     PLATFORM=generic \
     PLATFORM_RISCV_XLEN=32 \
     PLATFORM_RISCV_ISA=rv32ima_zicsr_zifencei \
     PLATFORM_RISCV_ABI=ilp32 \
     FW_FDT_PATH=$WORK_DIR/build/linux.dtb \
     FW_JUMP_ADDR=0x40000000  \
     FW_JUMP_FDT_ADDR=0x40ef0000 \
     -j20
```

## Prepare boot.json

```shell
cd $WORK_DIR
cat > boot.json << EOF
{
"Image":        "0x40000000",
"rootfs.cpio":  "0x41000000",
"opensbi.bin":  "0x40f00000"
}
EOF
```

## Format sdcard

```shell
export SDCARD=/dev/???
export SDCARD_P1=${SDCARD}p1
export SDCARD_P2=${SDCARD}p2

# Write the partition table
(
echo o
echo n
echo p
echo 1
echo
echo +100M
echo y
echo n
echo p
echo 2
echo
echo +300M
echo y
echo t
echo 1
echo b
echo p
echo w
) | sudo fdisk $SDCARD

sudo mkfs.vfat $SDCARD_P1
```

## Write the sdcard

```shell
cd $WORK_DIR
export BOOT=mnt_p1
mkdir -p $BOOT
sudo mount $SDCARD_P1 $BOOT
sudo cp boot.json $BOOT/boot.json
sudo cp opensbi/build/platform/generic/firmware/fw_jump.bin $BOOT/opensbi.bin
sudo cp buildroot/build/basic/images/Image $BOOT
sudo cp buildroot/build/basic/images/rootfs.cpio $BOOT
sudo umount $BOOT
```




