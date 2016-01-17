#!/bin/bash

function install() {
    rsync --exclude ".git/"  \
          --exclude ".DS_Store" \
          --exclude "install.sh" \
          --exclude "README.md" \
          --exclude "LICENSE" \
          --exclude "scripts/" \
          -avh --no-perms . $HOME

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
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    install
else
    read -p "This operation may override files in your home directory. Proceed? [y/n]"
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        install
    fi
fi

unset install
