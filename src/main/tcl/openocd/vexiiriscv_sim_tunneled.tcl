# SPDX-FileCopyrightText: 2023 "Everybody"
#
# SPDX-License-Identifier: MIT

adapter speed 10000
adapter driver remote_bitbang
remote_bitbang_host localhost
remote_bitbang_port 44853

set _CHIPNAME riscv
jtag newtap $_CHIPNAME cpu -irlen 6 -expected-id 0x10003FFF

set _TARGETNAME $_CHIPNAME.cpu

target create $_TARGETNAME.0 riscv -chain-position $_TARGETNAME
$_TARGETNAME.0 configure -work-area-phys 0x70000000 -work-area-size 10000 -work-area-backup 1

riscv use_bscan_tunnel 6 1

init
halt
echo "Done"
