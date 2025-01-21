for {set i 0} {$i < $cpu_count} {incr i} {
    targets $_TARGETNAME.$i
    halt
}

for {set i 0} {$i < $cpu_count} {incr i} {
    targets $_TARGETNAME.$i
    reg 2112 0x3
}

for {set i 0} {$i < $cpu_count} {incr i} {
    targets $_TARGETNAME.$i
    resume
}


