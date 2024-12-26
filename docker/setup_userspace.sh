#!/bin/bash

cd /home/ubuntu

git clone  --recursive https://github.com/SpinalHDL/VexiiRiscv.git

pushd VexiiRiscv/ext/riscv-isa-sim/fesvr
echo "Patching fesvr"
git apply /home/ubuntu/fix_fesvr.patch
popd

mkdir VexiiRiscv/ext/riscv-isa-sim/build
pushd VexiiRiscv/ext/riscv-isa-sim/build
../configure --prefix=/opt/riscv --enable-commitlog  --without-boost --without-boost-asio --without-boost-regex
make
popd

pushd VexiiRiscv/ext/rvls
make
popd

pushd VexiiRiscv
sbt update
popd

git clone https://github.com/shioyadan/konata.git
pushd konata
npm install
popd
