#!/bin/bash

# Install basic utilities and applications

# Check for Homebrew and install if necessary
if test ! $(which brew); then
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Update Homebrew recipes
brew update

# Update Unix tools
brew install coreutils
brew install findutils
brew install bash
brew tap homebrew/dupes
brew install homebrew/dupes/grep

# Other binaries
binaries=(
    python
    node
    ack
    git
    gpg2
    # ledger
    # pass
    tmux
    # pandoc
    # postgres
    reattach-to-user-namespace
)

echo "Installing binaries..."
brew install ${binaries[@]}

# Install updated text editors
brew install vim --override-system-vi --with-lua
hash -r
brew install emacs --srgb --with-cocoa --use-git-head

# Install English and Russian dictionaries
brew install aspell --with-lang-ru --with-lang-en

# Install translate CLI
brew install http://www.soimort.org/translate-shell/translate-shell.rb

brew cleanup

# Install apps
brew install caskroom/cask/brew-cask

apps=(
    anki
    google-backup-and-sync
    google-chrome
    iterm2
    slack
)

echo "Installing apps.."
brew cask install --appdir="/Applications" ${apps[@]}
