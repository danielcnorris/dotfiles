#!/usr/bin/bash
# Antergos
# Select base install with no add ons other than NTP for time syncronization.

# Manual steps
# Download this repo.
# Log into Dropbox and Chromium.
# Log into Anki.
# TODO Set up ssh keys (add command and ssh-agent steps)
# TODO Move these manual steps that are common into the common isntall script
sudo -v

PACMAN_PKGS=(
  anki
  chromium
  dwm
  ibus
  ibus-libpinyin
  git
  gvim
  go
  nodejs
  npm
  python-pip
  r
  redshift
  slock
  the_silver_surfer
  tmux
  ttf-inconsolata
  xclip
  xorg
  xorg-xbacklight
  xorg-xinit
  zsh
)
sudo pacman -Sy --noconfirm ${PACMAN_PKGS[@]}

# Install pacaur.
mkdir pacaur; cd pacaur  
curl https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=pacaur -o PKGBUILD  
makepkg -s --noconfirm
makepkg -i --noconfirm 
cd ..
rm -Rf pacaur
PACAUR_PKGS=(
  dropbox
  fzf
  st-solarized
)
pacaur -S --noconfirm ${PACAUR_PKGS[@]}

PYTHON_PKGS=(
  pep8
  pyflakes
  flake8
  requests
)
sudo pip install  $PYTHON_PKGS

# Remove things placed by Antergos.
rem -Rf ~/Desktop
rem -Rf ~/.gnome
