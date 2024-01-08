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


install_rvls(){
  cd $VEXIIRISCV/ext/rvls
  make -j$(nproc)
  cp -f build/apps/rvls.so ~/tools/rvls.so
}


install_elfio(){
  git clone https://github.com/serge1/ELFIO.git
  cd ELFIO
  git checkout d251da09a07dff40af0b63b8f6c8ae71d2d1938d # Avoid C++17
  sudo cp -R elfio /usr/include
  cd ..
}

install_packages(){
  sudo apt-get update
  sudo apt install -y zlib1g-dev libboost-all-dev libboost-dev libasio-dev device-tree-compiler libsdl2-2.0-0 libsdl2-dev
  install_elfio
}

install_uncached(){
  export VEXIIRISCV=${PWD}
  install_NaxSoftware

  mkdir -p $VEXIIRISCV/ext/rvls/build/apps
  cp -f ~/tools/rvls.so $VEXIIRISCV/ext/rvls/build/apps/rvls.so
}

install_cached(){
  export VEXIIRISCV=${PWD}
  mkdir -p ~/tools
  (install_spike)
  (install_rvls)
  (install_verilator)
}
