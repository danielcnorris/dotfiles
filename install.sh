#!/bin/bash

dir=$(pwd)
declare -a files=("bash_profile"
                  "bashrc"
                  "gitconfig"
                  "tmux.conf"
                  "vim"
                  "vimoutlinerrc"
                  "vimrc"
                  "xinitrc"
                  "xserverrc")

for file in "${files[@]}"
do
    unlink $HOME/.$file
    ln -s $dir/$file $HOME/.$file
done

if [ ! -d $HOME/.vim/bundle/Vundle.vim ]; then
    git clone https://github.com/gmarik/Vundle.vim.git \
        $HOME/.vim/bundle/Vundle.vim
fi

vim +PluginInstall +qall
source $HOME/.bash_profile
