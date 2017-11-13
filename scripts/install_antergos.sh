#!/bin/bash
# Antergos Linux distro.
# Select base install with no add ons other than NTP for time synchronization.

CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
cd ..
DOT_DIR=$(pwd)

sudo -v

PACMAN_PKGS=(
  acpi
  anki
  asp
  chromium
  ibus
  ibus-libpinyin
  ibus-m17n
  feh
  flake8
  git
  gvim
  go
  jhead
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
makepkg -si --noconfirm
cd ..
rm -Rf pacaur
PACAUR_PKGS=(
  drive
  dropbox
  fd-rs
  fzf
  sqlint
  st-solarized
)
pacaur -S --noconfirm ${PACAUR_PKGS[@]}

# Set up dwm.
# NOTE: You must manually edit /etc/pacman.conf to ignore dwm for regular
# pacman updates.
if [[ ! -d "$HOME/abs" ]]
then
  mkdir "$HOME/abs"
fi
cd "$HOME/abs"
asp checkout dwm
cp "$DOT_DIR/abs/dwm/config.h" "$HOME/abs/dwm/repos/community-x86_64/"
cd "$HOME/abs/dwm/repos/community-x86_64/"
makepkg -g >> PKGBUILD
makepkg -fsi --noconfirm
cd "$DOT_DIR"

# Set up root password.
su
passwd
exit

# Remove things placed by Antergos.
rm -Rf "$HOME/Desktop"
rm -Rf "$HOME~/.gnome"

cd "$CALLER_DIR"
