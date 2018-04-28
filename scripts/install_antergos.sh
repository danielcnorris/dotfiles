#!/bin/bash
# Antergos Linux distro.
# Select base install with no add ons other than NTP for time synchronization and printer support.
# TODO Internet setup... what does the below do?
# systemctl enable netctl-auto@wlan0.service

set -e

CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
cd ..
DOT_DIR=$(pwd)

sudo -v

PACMAN_PKGS=(
  acpi
  chromium
  feh
  flake8
  fzf
  git
  gvim
  go
  nodejs
  npm
  python-pip
  python-requests
  redshift
  ripgrep
  slock
  tmux
  ttf-hack
  xclip
  xbindkeys
  xorg
  xorg-xinit
  yapf
  zsh
)
sudo pacman -Sy --noconfirm ${PACMAN_PKGS[@]}

# Install pacaur.
mkdir pacaur; cd pacaur
curl https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=pacaur -o PKGBUILD
makepkg -si --noconfirm
cd ..
rm -Rf pacaur
PACAUR_PKGS=(
  st
  dmenu
)
pacaur -S --noconfirm ${PACAUR_PKGS[@]}

# Set up DWM.
if [[ ! -d "$HOME/dwm" ]]
then
  cd "$HOME"
  git clone https://git.suckless.org/dwm
fi
cd "$HOME/dwm"
git pull origin master
cp "$DOT_DIR/config.dwm.h" config.h
make clean install
cd "$DOT_DIR"

# Set up ST.
if [[ ! -d "$HOME/st" ]]
then
  cd "$HOME"
  git clone https://git.suckless.org/st
fi
cd "$HOME/st"
git pull origin master
cp "$DOT_DIR/config.st.h" config.h
make clean install
cd "$DOT_DIR"

# Set up root password.
su
passwd
exit

cd "$CALLER_DIR"
