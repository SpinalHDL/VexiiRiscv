# VexiiRiscv

VexiiRiscv is a from scratch second iteration of VexRiscv. Here are the targets : 

- RISCV 32/64 bits IMAFDC
- Could start around as small as VexRiscv, but could scale further in performance
- Optional multi issue
- Optional multi threading
- Cleaning implementation, especially the frontend
- ...

It's currently very very early, but you can run the generation via : 

```shell
git clone --recursive https://github.com/SpinalHDL/VexiiRiscv.git
cd VexiiRiscv
sbt "test:runMain vexiiriscv.scratchpad.Play1"
```