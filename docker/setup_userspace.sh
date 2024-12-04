#!/bin/bash

cd /home/ubuntu

git clone  --recursive https://github.com/SpinalHDL/VexiiRiscv.git

pushd VexiiRiscv
sbt update
popd

rm -rf VexiiRiscv

git clone https://github.com/shioyadan/konata.git
pushd konata
npm install
popd
