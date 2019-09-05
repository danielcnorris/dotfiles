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
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"

if [[ $(uname) = "Linux" ]]
then
  export BROWSER=/usr/bin/firefox
  NPM_PACKAGES="${HOME}/.npm"
  export PATH="$NPM_PACKAGES/bin:$PATH"
  unset MANPATH
  export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
  alias rd='redshift -x'
  alias rn='redshift -O 2000'
  alias f='feh --scale-down --auto-zoom'
  alias m='xrandr --output eDP1 --auto --output HDMI1 --auto --right-of eDP1'
  [[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
  [[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh
  [ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
  [ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
else
  export PATH="/usr/local/opt/python/libexec/bin:/Users/daniel/Library/Python/3.7/bin:$PATH"
  export D="$HOME/Google\ Drive/"
  export C="$GOPATH/src/github.com/caffeinetv/"
  [[ -f /Users/daniel/shadow/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/daniel/shadow/node_modules/tabtab/.completions/serverless.zsh
  [[ -f /Users/daniel/shadow/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/daniel/shadow/node_modules/tabtab/.completions/sls.zsh
fi

export FZF_DEFAULT_OPTS="--color bw"
export FZF_DEFAULT_COMMAND='rg --files -g "!{node_modules,vendor}/*"'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
