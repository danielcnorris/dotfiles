#!/usr/bin/zsh
# TODO Invocation path will probably be different on macos.

# Prezto.
rm -Rf "${ZDOTDIR:-$HOME}/.zprezto"
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  unlink "${ZDOTDIR:-$HOME}/.${rcfile:t}"
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
chsh -s /usr/bin/zsh

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

dir=$(pwd)
declare -a files=("asoundrc"
                  "gitconfig"
                  "tmux.conf"
                  "vim"
                  "vimoutlinerrc"
                  "vimrc"
                  "xinitrc"
                  "xserverrc"
                  "xbindkeysrc"
		  "zlogin"
		  "zprestorc"
		  "zshrc")

for file in "${files[@]}"
do
    unlink $HOME/.$file
    ln -s $dir/$file $HOME/.$file
done

# vim-plug
if [ ! -d $HOME/.vim/autoload/plug.vim ]; then
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

vim +PlugInstall
source $HOME/.zshrc
