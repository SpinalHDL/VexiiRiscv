# Getting Alpine linux to run on RISC-V

So this README compile a set of command lines which were used to get it up on running on the VexiiRiscv RV64GC litex SoC.
You can find most of the fundamental informations on the links bellow : 

- https://blog.ari.lt/b/how-to-manually-install-alpine-linux-on-any-linux-distribution/
- https://wiki.gentoo.org/wiki/OpenRC_to_systemd_Cheatsheet

Overall, if you have a system which can run Debian, then it should be able to run Alpine without issues.
The one tricky thing with alpine linux is that the image you can download generaly need a few tweeks using chroot to be functional.

## Image setup 

```bash
cd /media/rawrr/rootfs
sudo rm -rf *
sudo tar xpvf /media/data2/download/alpine-minirootfs-3.20.1-riscv64.tar.gz --xattrs-include='*.*' --numeric-owner
sudo chroot /media/rawrr/rootfs /bin/ash

echo -e 'nameserver 8.8.8.8\nnameserver 2620:0:ccc::2' > ${chroot_dir}/etc/resolv.conf	
apk update
apk add alpine-conf openrc kbd-bkeymaps kbd openssh-server nano chronyd openssh-client-common --no-cache
sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
rc-update add sshd boot
rc-update add loadkeys boot
rc-update add chronyd boot

apk add xf86-video-fbdev xterm
```

## Scrap commands

```bash
setup-keymap ch fr
XKBMODEL="pc105"
XKBLAYOUT="ch"
XKBVARIANT="fr"
XKBOPTIONS=""

mount -o remount,rw /

kbd-bkeymaps

loadkeys ch-fr

apk add chocolate-doom --repository=https://dl-cdn.alpinelinux.org/alpine/edge/testing
```
