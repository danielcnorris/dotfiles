# User setup for new Arch Linux installation.
#!/bin/bash

sudo -v

# Install packman packages
PACMAN_PKGS=(
	vim
	tmux
	git
	xorg
	xorg-xinit
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
	python-pip
	r
	openssh
	xclip
  # Latex?
  abs
  remind
  wpa_actiond
)

# sudo pacman -Sy --noconfirm ${PACMAN_PKGS[@]}

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


# Python packages
# Set up ST with git
# TODO Edit /etc/makepkg.conf
# TODO Make abs build repository
