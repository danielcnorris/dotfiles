# User setup for new Arch Linux installation.
#!/bin/bash

sudo -v

# Set the language.
localectl set-locale LANG="en_US.utf8"

# Install packman packages.
PACMAN_PKGS=(
  abs
  acpi
  alsa-utils
  chromium
  cmus
  dosfstools
  dwm
  expac
  feh
  git
  go
  gptfdisk
  gvim
  gxmessage
  ibus
  ibus-libpinyin
  ibus-m17n
  jshon
  mplayer
  mps-youtube
  mpv
  mutt
  nodejs
  npm
  ntp
  openssh
  pandoc
  perl-image-exiftool
  perl-file-copy-recursive
  perl-io-all
  perl-test-warn
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
  wqy-zenhei
  wyrd
  w3m
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
  anki
  chromium-widevine
  fdupes
  findimagedupes
  goobook
  gcalcli
  go-tools
  masterpdfeditor
  urlview
  zramswap
)

packer -S --noconfirm ${PACKER_PKGS[@]}

# Configure touchpad.
sudo cp 70-synaptics.conf /etc/X11/xorg.conf.d/

# TODO Edit /etc/makepkg.conf
# TODO Make abs build repository
# TODO Run makepkg and install abs packages

# TODO Skip abs packages on pacman update.
# IgnoreGroup = modified in /etc/pacman.conf

# TODO Set up systemd services
# NTPD, zram, backup service
PYTHON_PKGS=(
  pep8
  pyflakes
  flake8
  requests
  youtube-dl
)
sudo pip install  $PYTHON_PKGS

# TODO R packages: stringr, tidyverse
