#!/bin/bash
Xvfb :1 -screen 0 1024x768x16 &> /work/xvfb.log &
until pids=$(pidof Xvfb)
do   
    sleep 1
done
x11vnc -display :1 -bg -forever -nopw -quiet -xkb
touch /work/vnc_done
startxfce4 &> /work/docker_xfce4.lop
