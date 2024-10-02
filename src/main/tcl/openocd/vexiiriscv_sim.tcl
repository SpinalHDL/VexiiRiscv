# SPDX-FileCopyrightText: 2023 "Everybody"
#
# SPDX-License-Identifier: MIT

adapter speed 10000
adapter driver remote_bitbang
remote_bitbang_host localhost
remote_bitbang_port 44853

set _CHIPNAME riscv
jtag newtap $_CHIPNAME cpu -irlen 5 -expected-id 0x10002FFF

set _TARGETNAME $_CHIPNAME.cpu

target create $_TARGETNAME.0 riscv -chain-position $_TARGETNAME
poll_period 400

init
halt

echo "Ready for Remote Connections"

