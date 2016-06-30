# User setup for new Arch Linux installation.
#!/bin/bash

sudo -v

# Install packman packages
PACMAN_PKGS=(
	gvim
	tmux
	git
	xorg
	xorg-xinit
  xorg-xbacklight
  xbindkeys
	dwm
	chromium
	wget
	expac
	jshon
	slock
	alsa-utils
	acpi
	go
	nodejs
  npm
	python-pip
	r
	openssh
	xclip
  abs
  remind
  wpa_actiond
  dosfstools
  syslinux
  gptfdisk
)

sudo pacman -Sy --noconfirm ${PACMAN_PKGS[@]}

# Install Packer.
install_packer() {
	mkdir packer
	cd packer
	sudo wget https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=packer
	mv PKGBUILD?h=packer PKGBUILD
	makepkg -i --noconfirm
	cd ..
	sudo rm -Rf packer
}

install_packer

# Install Packer packages.
PACKER_PKGS=(
	chromium-widevine
)

packer -S --noconfirm ${PACKER_PKGS[@]}

# Configure touchpad.
sudo cp 70-synaptics.conf /etc/X11/xorg.conf.d/

# TODO Edit /etc/makepkg.conf
# TODO Make abs build repository
# TODO Run makepkg and install abs packages

# TODO Skip abs packages on pacman update.
# IgnoreGroup = modified in /etc/pacman.conf
