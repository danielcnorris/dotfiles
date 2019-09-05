#!/usr/bin/zsh
# Manual steps
# Download this repo.
# Log into Chromium.
# Create SSH keys, add to Github, and add to ssh-agent.
# https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
# Configure AWS: aws configure

CALLER_DIR=$(pwd)
cd "$(dirname "$0")"
cd ..

# Prezto.
if [[ ! -d "${ZDOTDIR:-$HOME}/.zprezto" ]]
then
  git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
  setopt EXTENDED_GLOB
  for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
    unlink "${ZDOTDIR:-$HOME}/.${rcfile:t}"
    ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
  done
  chsh -s /usr/bin/zsh
else
  git -C "${ZDOTDIR:-$HOME}/.zprezto" pull
  git -C "${ZDOTDIR:-$HOME}/.zprezto" submodule update --init --recursive
fi

DIR=$(pwd)
FILES=(
  emacs.d
  gitconfig
  gitignore
  tmux.conf
  vim
  vimrc
  zlogin
  zpreztorc
  zshrc
)

if [[ $(uname) = "Linux" ]]
then
  FILES+=(
    Xmodmap
  )
fi

for FILE in "${FILES[@]}"
do
  unlink "$HOME/.$FILE"
  ln -s "$DIR/$FILE" "$HOME/.$FILE"
done

unlink "$HOME/.config/flake8"
ln -s "$DIR/flake8" "$HOME/.config/flake8"

source "$HOME/.zshrc"

PIP_PKGS=(
  awscli
  black
  requests
)

pip install --user ${PIP_PKGS[@]}

NPM_PKGS=(
  babel-eslint
  create-react-app
  prettier
  serverless
)

# https://github.com/sindresorhus/guides/blob/master/npm-global-without-sudo.md
mkdir "${HOME}/.npm"
npm config set prefix "${HOME}/.npm"
npm install -g ${NPM_PKGS[@]}

# TODO Install go packages
# go get -u honnef.co/go/tools/cmd/...

cd "$CALLER_DIR"
