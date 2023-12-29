#!/bin/bash

# SPDX-FileCopyrightText: 2023 "Everybody"
#
# SPDX-License-Identifier: MIT

install_verilator(){
  sudo apt-get update
  sudo apt install -y git make autoconf g++ flex libfl-dev bison  # First time prerequisites
  git clone http://git.veripool.org/git/verilator   # Only first time
  unset VERILATOR_ROOT  # For bash
  cd verilator
  git pull        # Make sure we're up-to-date
  git checkout v4.216
  autoconf        # Create ./configure script
  ./configure --prefix ~/tools
  make -j$(nproc)
  make install
  cd ..
}



install_NaxSoftware(){
  (cd $VEXIIRISCV/ext/NaxSoftware  && ./init.sh)
}

install_spike(){
  cd $VEXIIRISCV/ext/riscv-isa-sim
  mkdir build
  cd build
  ../configure --prefix=$RISCV --enable-commitlog --without-boost --without-boost-asio --without-boost-regex
  make -j$(nproc)
}

install_elfio(){
  git clone https://github.com/serge1/ELFIO.git
  cd ELFIO
  sudo cp -R elfio /usr/include
  #export C_INCLUDE_PATH=${PWD}/elfio
  cd ..
}

install_packages(){
  sudo apt-get update
  sudo apt install -y zlib1g-dev libboost-all-dev libboost-dev libasio-dev device-tree-compiler libsdl2-2.0-0 libsdl2-dev
}

install_uncached(){
  export VEXIIRISCV=${PWD}
  install_elfio
  install_NaxSoftware

  mkdir -p $VEXIIRISCV/ext/riscv-isa-sim/build
  cp -f ~/tools/spike.so $VEXIIRISCV/ext/riscv-isa-sim/build/package.so
  cp -f ~/tools/spike.h $VEXIIRISCV/ext/riscv-isa-sim/build/config.h
}

install_cached(){
  export VEXIIRISCV=${PWD}
  mkdir -p ~/tools
  (install_spike)
  (install_verilator)
}
