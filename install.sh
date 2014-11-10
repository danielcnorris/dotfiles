#!/bin/bash

function install() {
    rsync --exclude ".git/"  \
          --exclude ".DS_Store" \
          --exclude "install.sh" \
          -avh --no-perms . $HOME
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
