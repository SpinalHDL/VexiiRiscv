#!/bin/bash
docker pull leviathanch/vexiiriscv
container_id=$(docker run -v `pwd`:/work --privileged=true -idt leviathanch/vexiiriscv)
address=$(docker inspect \
  -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $container_id)
echo "Waiting for the VNC server to come up"
rm -f vnc_done
until [ -f vnc_done ]
do
    sleep 1
done
echo "Address: [$address]"
vncviewer $address
