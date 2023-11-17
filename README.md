# VexiiRiscv

VexiiRiscv is a from scratch second iteration of VexRiscv. Here are the targets : 

- RISCV 32/64 bits IMAFDC
- Could start around as small as VexRiscv, but could scale further in performance
- Optional multi issue
- Optional multi threading
- Cleaning implementation, especially the frontend
- ...

# Setup

```shell
git clone --recursive https://github.com/SpinalHDL/VexiiRiscv.git
cd VexiiRiscv
```

# Generate Verilog

It's currently very very early, but you can run the generation via : 

```shell
sbt "test:runMain vexiiriscv.scratchpad.Play1"
```

# Run a simulation

```shell
(cd ext/NaxSoftware/baremetal/vexiiriscv && make rv32ima)
sbt " test:runMain vexiiriscv.tester.TestBench --load-elf ext/NaxSoftware/baremetal/vexiiriscv/build/rv32ima/vexiiriscv.elf  --passAfter 1000 --no-rvls"
```