#!/bin/bash

dir=$(pwd)
declare -a files=("bash_profile"
                  "bashrc"
                  "emacs.d"
                  "ghci"
                  "gitconfig"
                  "tmux.conf"
                  "vim"
                  "vimoutlinerrc"
                  "vimrc")

for file in "${files[@]}"
do
    unlink $HOME/.$file
    ln -s $dir/$file $HOME/.$file
done

if [ ! -d $HOME/.vim/bundle/Vundle.vim ]; then
    git clone https://github.com/gmarik/Vundle.vim.git \
        $HOME/.vim/bundle/Vundle.vim
fi

echo "" >> $HOME/.gitconfig
echo "[user]" >> $HOME/.gitconfig
echo "    name = $GITUSER" >> $HOME/.gitconfig
echo "    email = $GITEMAIL" >> $HOME/.gitconfig

vim +PluginInstall +qall
source $HOME/.bash_profile

cp "$SECRETS/secrets.el" $HOME/.emacs.d/lisp/
