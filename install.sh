#!/bin/bash
# TODO
# Need to upgrade Vim: brew update
# brew install vim --override-system-vi --with-lua
# hash -r

function install() {
    rsync --exclude ".git/"  \
          --exclude ".DS_Store" \
          --exclude "install.sh" \
          --exclude "README.md" \
          --exclude "LICENSE"
          -avh --no-perms . $HOME

    if [ ! -d $HOME/.vim/bundle/Vundle.vim ]; then
        git clone https://github.com/gmarik/Vundle.vim.git \
            $HOME/.vim/bundle/Vundle.vim
    fi

    vim +PluginInstall +qall
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
