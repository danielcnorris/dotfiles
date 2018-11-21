# Manjaros Linux distro. Use Manjaro Architect and select i3 edition.

set -e

CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
cd ..
DOT_DIR=$(pwd)

sudo -v

PACMAN_PKGS=(
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
  slock
  tmux
  xclip
)
sudo pacman -Sy --noconfirm ${PACMAN_PKGS[@]}

# Perform after Emacs installation.
sudo pacman -Sy --noconfirm ctags

yaourt -Sy pacaur

PACAUR_PKGS=(
  leiningen
)
pacaur -Sy --noconfirm --noedit ${PACMAN_PKGS[@]}

sudo systemctl enable --now docker.service

# Manjaro creates default versions of these files.
cat "$DIR/config.i3" >> "$HOME/i3/config"
sed -i "/^URxvt\.font/c\URxvt\.font: xft:Deja Vu Sans Mono:size=10" "$HOME/.Xresources"

cd "$CALLER_DIR"
