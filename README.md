# VexiiRiscv

VexiiRiscv (Vex2Risc5) is the successor of VexRiscv. Work in progress, here are its currently implemented features :

- RV32/64 I[M][A][F][D][C][S][U][B]
- Up to 5.24 coremark/Mhz 2.50 dhystone/Mhz (WIP)
- In-order execution
- early [late-alu]
- single/dual issue (can be asymmetric)
- BTB, GShare, RAS branch prediction
- cacheless fetch/load/store, I$, D$ (WIP)
- optional SV32/SV39 MMU
- Can run linux / buildroot
- Pipeline visualisation in simulation via Konata
- Lock step simulation via RVLS and Spike

Overall the goal is to have a design which can stretch (through configuration) from Cortex M0 up to a Cortex A53 and potentialy beyond.

Here is the online documentation : 

- https://spinalhdl.github.io/VexiiRiscv-RTD/master/VexiiRiscv/Introduction/#
- https://spinalhdl.github.io/VexiiRiscv-RTD/master/VexiiRiscv/HowToUse/index.html

A roadmap is available here : 

- https://github.com/SpinalHDL/VexiiRiscv/issues/1

# TL;DR Getting started

The quickest way for getting started is to pull the Docker image with all the dependencies installed

    docker pull leviathanch/vexiiriscv

Then you can directly get the most recent Verilog code by running

    git clone --recursive https://github.com/SpinalHDL/VexiiRiscv.git
    cd VexiiRiscv
    docker run -v `pwd`:/work -it leviathanch/vexiiriscv
    sbt "Test/runMain vexiiriscv.Generate"

After running the generation command you'll find a file named "VexiiRiscv.v" in the root
of the repository folder, which you can drag into your Quartus or whatever.
We decided to not start covering FPGAs because there's just too many, so it's up to you
to define your pin configuration for your specific FPGA

If you want to know what else you can do with sbt, please refer to the complete documentation.

# Rebuild the Docker container

In case you wanna rebuild leviathan's Docker container you can run

    docker build . -f docker/Dockerfile -t vexiiriscv --progress=plain
