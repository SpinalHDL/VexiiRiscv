## Introduction

This readme document (from scratch) how to generate all the images to run debian Litex + VexiiRiscv.

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
sudo apt install debootstrap qemu qemu-user-static binfmt-support dpkg-cross debian-archive-keyring --no-install-recommends
sudo apt install mmdebstrap qemu-user-static binfmt-support 
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

## Create a Debian rootfs

```shell
cd $WORK_DIR
export CHROOT_DIR=/tmp/riscv64-chroot

# install base files, note that the key added bellow may change
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 0E98404D386FA1D9
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 6ED0E7B82643E131
sudo mmdebstrap --architectures=riscv64 sid $CHROOT_DIR "deb http://deb.debian.org/debian sid main" 
# chroot into base filesystem and made basic configuration
sudo chroot $CHROOT_DIR

# Update package information
apt-get update
apt-get --fix-broken install

# Set up basic networking
cat > /etc/network/interfaces <<EOF
auto lo
iface lo inet loopback

allow-hotplug eth0
iface eth0 inet dhcp
EOF

# Set root password 
passwd

# Change hostname
echo miaou > /etc/hostname

# Set up fstab
cat > /etc/fstab <<EOF
# <file system> <mount point>   <type>  <options>       <dump>  <pass>

/dev/mmcblk0p2 /               ext4    errors=remount-ro 0       1
/dev/mmcblk0p1 /boot           vfat    nodev,noexec,rw   0       2
EOF


# Install networking stuff, note the PermitRootLogin to allow SSH root login
apt-get -y install openssh-server openntpd ntpdate net-tools avahi-daemon avahi-utils
sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config

# Install a few utilities
apt-get -y install sl hdparm htop wget psmisc tmux kbd usbutils

# Install stuff allowing to compile stuff directly from the target
apt-get -y install gcc git libncursesw5-dev autotools-dev autoconf automake libtool build-essential

# x11 related stuff
apt-get -y install xorg xserver-xorg-core xinit
apt-get -y install twm wmaker
# GLX is very very slow as soon as X11 app start to use pixel based buffer, need to disable it.
cat >> /etc/X11/xorg.conf <<EOF
Section "Extensions"
    Option "GLX" "Disable"
EndSection
EOF

# Multimedia
apt-get -y install mpg123 ffmpeg
apt-get -y install chocolate-doom openttd xscreensaver xscreensaver-data xscreensaver-data-extra
echo export SDL_VIDEODRIVER=x11 >> /root/.bashrc


apt-get clean

# exit chroot
exit

sudo umount $CHROOT_DIR
```

Note if you use linux DRI, you need to add the following to the xorg.conf : 

```
Section "Device"
    Identifier "Modesetting"
    Driver "modesetting"
    Option "AccelMethod" "none"
EndSection
```

# Compile linux

```shell
cd $WORK_DIR
git clone https://github.com/Dolu1990/litex-linux.git
cd litex-linux
git checkout f5ee078b
cp $WORK_DIR/VexiiRiscv/doc/litex/debian/linux.config .config

export CROSS_COMPILE=riscv64-unknown-linux-gnu-
make -j$(nproc) ARCH=riscv oldconfig
make -j$(nproc) ARCH=riscv all

ls vmlinux               # Kernel elf (has symboles, usefull for debug)
ls arch/riscv/boot/Image # Kernel binary

cd ..
unset CROSS_COMPILE
```

## Generate the hardware

Note that not all VexiiRiscv feature are enabled by default. For instance, you can add to the parameters bellow : 
- --vexii-args="--regfile-async --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --lsu-l1-store-buffer-slots=2" 
- --l2-byte=262144 --l2-self-flush=40c00000,40dd4c00,1666666


```shell
cd $WORK_DIR
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv --cpu-variant=debian  --bus-standard axi-lite \
--cpu-count=2 --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma  \
--soc-json build/csr.json --build

# Generate dts/dtb
python3 -m litex.tools.litex_json2dts_linux build/csr.json --root-device=mmcblk0p2 | grep -v "linux,initrd" > build/linux.dts
dtc -O dtb -o build/linux.dtb build/linux.dts
```

## Load the hardware

```shell
cd $WORK_DIR
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv --cpu-variant=debian  --bus-standard axi-lite \
--cpu-count=2 --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma  \
--soc-json build/csr.json --load
```

## Build opensbi

```shell
cd $WORK_DIR
git clone https://github.com/riscv-software-src/opensbi.git
cd opensbi
make CROSS_COMPILE=riscv64-unknown-linux-gnu- \
     PLATFORM=generic \
     PLATFORM_RISCV_XLEN=64 \
     PLATFORM_RISCV_ISA=rv64gc_zicsr_zifencei \
     PLATFORM_RISCV_ABI=lp64d \
     FW_FDT_PATH=$WORK_DIR/build/linux.dtb \
     FW_JUMP_ADDR=0x41000000  \
     FW_JUMP_FDT_ADDR=0x46000000 \
     -j20
```

## Prepare boot.json

```shell
cd $WORK_DIR
cat > boot.json << EOF
{
"Image":        "0x41000000",
"opensbi.bin":  "0x40C00000"
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
echo +500M
echo y
echo n
echo p
echo 2
echo
echo +14000M
echo y
echo t
echo 1
echo b
echo p
echo w
) | sudo fdisk $SDCARD

sudo mkfs.vfat $SDCARD_P1
sudo mkfs.ext4 $SDCARD_P2
sudo e2label $SDCARD_P2 rootfs

```

## Write the sdcard

```shell
cd $WORK_DIR
export P1=mnt_p1
mkdir -p $P1
sudo mount $SDCARD_P1 $P1
sudo cp boot.json $P1/boot.json
sudo cp opensbi/build/platform/generic/firmware/fw_jump.bin $P1/opensbi.bin
sudo cp litex-linux/arch/riscv/boot/Image $P1
sudo umount $P1


export P2=mnt_p2
mkdir -p $P2
sudo mount $SDCARD_P2 $P2
sudo rsync -aAX --progress $CHROOT_DIR/  $P2/
sudo umount $P2
```




