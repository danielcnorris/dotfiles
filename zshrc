#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
export EDITOR=vim
export VISUAL=vim

alias gfu="git pull upstream master"

export GOPATH="$HOME/go"
export G="$GOPATH/src/bitbucket.org/danielcnorris"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"

if [[ $(uname) = "Linux" ]]
then
  alias rd='redshift -O 5500'
  alias rn='redshift -O 2000'
  alias f='feh --scale-down --auto-zoom'
  alias m='xrandr --output eDP1 --auto --output HDMI1 --auto --right-of eDP1'
else
  export D="$HOME/Google\ Drive/"
  export C="$GOPATH/src/github.com/caffeinetv/"
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS="--color bw"
export FZF_DEFAULT_COMMAND='rg --files -g "!{node_modules,vendor}/*"'

# export PATH="$PATH:$HOME/.rvm/bin"

[[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
[[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh
