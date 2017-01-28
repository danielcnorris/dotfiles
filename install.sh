#!/bin/bash

dir=$(pwd)
declare -a files=("asoundrc"
                  "bash_profile"
                  "bashrc"
                  "gitconfig"
                  "goobookrc"
                  "mutt"
                  "muttrc"
                  "newsbeuter"
                  "tmux.conf"
                  "vim"
                  "vimoutlinerrc"
                  "vimrc"
                  "wyrdrc"
                  "xinitrc"
                  "xserverrc"
                  "xbindkeysrc")

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
