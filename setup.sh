# User setup for new Arch Linux installation.
#!/bin/bash

sudo -v

# Set the language.
localectl set-locale LANG="en_US.utf8"

# Install packman packages.
PACMAN_PKGS=(
  abs
  anki
	acpi
	alsa-utils
	chromium
  dosfstools
	dwm
	expac
	git
	go
  gptfdisk
	gvim
	jshon
  lynx
  mutt
	nodejs
  npm
  noto-fonts-cjk
	openssh
  pandoc
	python-pip
	r
  remind
	slock
  syslinux
  texlive-bin
  texlive-core
	tmux
	wget
  wpa_actiond
  xbindkeys
	xclip
	xorg
	xorg-xinit
  xorg-xbacklight
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
  goobook
  go-tools
  urlview
)

packer -S --noconfirm ${PACKER_PKGS[@]}

# Configure touchpad.
sudo cp 70-synaptics.conf /etc/X11/xorg.conf.d/

# TODO Edit /etc/makepkg.conf
# TODO Make abs build repository
# TODO Run makepkg and install abs packages

# TODO Skip abs packages on pacman update.
# IgnoreGroup = modified in /etc/pacman.conf
PYTHON_PKGS=(
  pep8
  pyflakes
  flake8
  requests
)
sudo pip install  $PYTHON_PKGS
