#!/bin/zsh

getdir() {
  echo $1 | cut -d'/' -f2
}

install_plugin() {
  git clone https://github.com/$1.git
}

update_plugin() {
  cd $1
  git pull origin master
  cd ..
}

CALLER_DIR=$(pwd)
mkdir -p $HOME/.vim/pack/plugins/start/
cd $HOME/.vim/pack/plugins/start/
PLUGINS=(
  junegunn/fzf.vim
  sheerun/vim-polyglot
  tpope/vim-commentary
  tpope/vim-eunuch
  tpope/vim-fugitive
  tpope/vim-repeat
  tpope/vim-rhubarb
  tpope/vim-sensible
  tpope/vim-sleuth
  tpope/vim-surround
  dense-analysis/ale
)
for PLUGIN in "${PLUGINS[@]}"
do
  DIR=$(getdir $PLUGIN)
  if [[ ! -d "$DIR" ]]
  then
    install_plugin $PLUGIN
  else
    update_plugin $DIR
  fi
done

vim -c 'helptags ALL|q'

cd "$CALLER_DIR"
