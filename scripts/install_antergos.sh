#!/bin/bash
# Antergos Linux distro.
# Select base install with no add ons other than NTP for time synchronization.

CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
cd ..
DOT_DIR=$(pwd)

sudo -v

PACMAN_PKGS=(
  anki
  asp
  chromium
  ibus
  ibus-libpinyin
  flake8
  git
  gvim
  go
  nodejs
  npm
  python-pip
  python-requests
  r
  redshift
  slock
  the_silver_searcher
  tmux
  ttf-inconsolata
  xclip
  xbindkeys
  xorg
  xorg-xbacklight
  xorg-xinit
  yapf
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
  sqlint
  st-solarized
)
pacaur -S --noconfirm ${PACAUR_PKGS[@]}

# Set up dwm.
if [[ ! -d "$HOME/abs" ]]
then
  mkdir "$HOME/abs"
fi
cd "$HOME/abs"
asp checkout dwm
cp "$DOT_DIR/abs/dwm/config.h" "$HOME/abs/dwm/repos/community-x86_64/"
cd "$HOME/abs/dwm/repos/community-x86_64/"
makepkg -g >> PKGBUILD
makepkg -si
cd "$DOT_DIR"

# Remove things placed by Antergos.
rm -Rf "$HOME/Desktop"
rm -Rf "$HOME~/.gnome"

cd "$CALLER_DIR"
