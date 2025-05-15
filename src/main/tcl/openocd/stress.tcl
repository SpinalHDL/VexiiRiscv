
for {set i 0} {$i < 100000} {incr i} {
    halt
    for {set s 0} {$s < 10} {incr s} {
        step
        echo [reg pc]
    }
    resume
    sleep 10
}

