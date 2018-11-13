xcode-select --install
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

binaries=(
  editorconfig
  go
  ispell
  mysql
  node
  postgresql
  python
  reattach-to-user-namespace
  ripgrep
  stack
  tmux
  fzf
)

brew install ${binaries[@]}

brew services start --all

# TODO Are these options still necessary?
brew install vim --override-system-vi --with-lua

brew tap caskroom/cask

apps=(
  docker
  google-chrome
  iterm2
  google-backup-and-sync
  # TODO Uninstall system emacs.
  emacs
)

brew cask install --appdir="/Applications" ${apps[@]}

# Think this has to happen after Emacs.
brew install --HEAD universal-ctags/universal-ctags/universal-ctags

# https://help.github.com/enterprise/2.13/user/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
ssh-keygen -t rsa -b 4096 -C "danielcnorris@gmail.com"
eval "$(ssh-agent -s)"
ssh-add -K ~/.ssh/id_rsa
