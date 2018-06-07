xcode-select --install
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

binaries=(
  go
  mysql
  node
  postgresql
  python
  reattach-to-user-namespace
  ripgrep
  stack
  tmux
)

brew install ${binaries[@]}

brew services start --all

brew install vim --override-system-vi --with-lua --with-python3
brew install --HEAD universal-ctags/universal-ctags/universal-ctags

brew tap caskroom/cask

apps=(
  docker
  google-chrome
  iterm2
)

brew cask install --appdir="/Applications" ${apps[@]}

# https://help.github.com/enterprise/2.13/user/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
ssh-keygen -t rsa -b 4096 -C "danielcnorris@gmail.com"
eval "$(ssh-agent -s)"
ssh-add -K ~/.ssh/id_rsa
