# Manjaros Linux distro. Use Manjaro Architect and select i3 edition.

set -e

CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
cd ..
DOT_DIR=$(pwd)

sudo -v

PACMAN_PKGS=(
  aspell
  aspell-en
  chromium
  clojure
  docker
  emacs
  flake8
  fzf
  go
  gvim
  libevent # Needed for tmux to work.
  nodejs
  npm
  pass
  python-pip
  redshift
  ripgrep
  slock
  tmux
  udisks2
  xclip
  yay
)
sudo pacman -Sy ${PACMAN_PKGS[@]}

# Perform after Emacs installation.
sudo pacman -Sy --noconfirm ctags

PACAUR_PKGS=(
  leiningen
)
pacaur -Sy --noconfirm --noedit ${PACMAN_PKGS[@]}

sudo systemctl enable --now docker.service--noconfirm

# Manjaro creates default versions of these files.
cat "$DIR/config.i3" >> "$HOME/i3/config"
sed -i "/^URxvt\.font/c\URxvt\.font: xft:Deja Vu Sans Mono:size=10" "$HOME/.Xresources"
# Change default browser
# https://www.reddit.com/r/ManjaroLinux/comments/7k8xi6/how_to_set_the_default_browser/

cd "$CALLER_DIR"
