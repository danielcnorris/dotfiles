# Manjaros Linux distro. Now using XFCE edition.

set -e

CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
cd ..
DOT_DIR=$(pwd)

sudo -v

PACMAN_PKGS=(
  aspell-en
  chromium
  clojure
  docker
  emacs
  flake8
  fzf
  go
  gvim
  nodejs
  npm
  pass
  python-pip
  redshift
  ripgrep
  tmux
  xclip
  yay
)
sudo pacman -Sy ${PACMAN_PKGS[@]}

# Perform after Emacs installation.
sudo pacman -Sy --noconfirm ctags

YAY_PKGS=(
  leiningen
)
yay -Sy --noconfirm ${YAY_PKGS[@]}

sudo systemctl enable --now docker.service

# TODO Map caps to ctrl.
# XFCE Terminal > Edit > Preferences > Appearance > Opacity to 100%.
# XFCE Terminal > Edit > Preferences > Colors > Dark Pastel
# XFCE Terminal Preferences > Compatability > Backspace generates ASCII DEL
# XFCE Terminal Preferences > Advanced > Disable all menu access keys
# If you want to change the default browser:
# https://www.reddit.com/r/ManjaroLinux/comments/7k8xi6/how_to_set_the_default_browser/
# Enable Emacs keybindings in GTK: https://wiki.archlinux.org/index.php/GTK#Keyboard_shortcuts

cd "$CALLER_DIR"
