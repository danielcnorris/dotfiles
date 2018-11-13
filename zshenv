export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"

if [[ $(uname) = "Darwin" ]]
then
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
