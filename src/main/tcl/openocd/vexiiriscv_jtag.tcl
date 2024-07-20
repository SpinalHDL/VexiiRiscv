# SPDX-FileCopyrightText: 2023 "Everybody"
#
# SPDX-License-Identifier: MIT

set _CHIPNAME riscv
set _TARGETNAME $_CHIPNAME.cpu
set cpu_count 1
if [info exists env(HART_COUNT)]  {
    set cpu_count $::env(HART_COUNT)
}

jtag newtap $_CHIPNAME cpu -irlen 5 -expected-id 0x10002FFF

for {set i 0} {$i < $cpu_count} {incr i} {
  target create $_TARGETNAME.$i riscv -coreid $i -chain-position $_TARGETNAME
    riscv expose_csrs 2047
}

for {set i 0} {$i < $cpu_count} {incr i} {
    targets $_TARGETNAME.$i
    init
    halt
}

targets riscv.cpu.0
echo "Ready for Remote Connections"

