#!/bin/zsh
# Manual steps
# Download this repo.
# Log into Chromium.
# Create SSH keys, add to Github, and add to ssh-agent.
# https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
# TODO Install standard-prettier.
# TODO Install AWS CLI, black.

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
  if [[ $(uname) = "Darwin" ]]
  then
    chsh -s /usr/bin/zsh
  fi
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

source "$HOME/.zshrc"
cd "$CALLER_DIR"
