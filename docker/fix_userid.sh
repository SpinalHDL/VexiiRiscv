#!/bin/bash
nuid=$(stat -c "%u" /work)
ouid=$(id -u ubuntu)

if [ "$nuid" -ne "$ouid" ]; then
    echo "Adjusting user access rights... please wait"
    usermod -u $nuid ubuntu
    chown ubuntu -R /home/ubuntu
fi

chown root /home/ubuntu/konata/node_modules/electron/dist/chrome-sandbox
chmod 4755 /home/ubuntu/konata/node_modules/electron/dist/chrome-sandbox
