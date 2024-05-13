# VexiiRiscv

VexiiRiscv (Vex2Risc5) is the successor of VexRiscv. Work in progress, here are its currently implemented features :

- RV32/64 I[M][A][C][S][U]
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
