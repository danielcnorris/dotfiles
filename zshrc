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

alias gua="git remote add upstream"
alias gfu="git pull upstream master"

note() {
  ts=$(date +"%s")
  entry="/tmp/entry-$ts.md"
  display_ts="[$(date +"%F %a %R")]"
  echo "#  " > $entry
  $EDITOR -c "normal $" +startinsert $entry
  file=${1:-notes.md}
  title=$(head -n1 $entry)
  content=$(tail -n+2 $entry)
  echo -e "$title\n$display_ts\n$content\n" >> $file
}

if [[ $(uname) = "Linux" ]]
then
  alias rd='redshift -O 5500'
  alias rn='redshift -O 2000'
  alias f='feh --scale-down --auto-zoom'
  alias m='xrandr --output eDP1 --auto --output HDMI1 --auto --right-of eDP1'
  export GOPATH="$HOME/go"
  export G="$GOPATH/src/bitbucket.org/danielcnorris"
  export PATH="$PATH:/home/dcn/.gem/ruby/2.4.0/bin"
else
  export D="$HOME/Google\ Drive/dcn"
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
  source "$HOME/.tmux-macos.conf"
  source "${HOME}/.zgen/zgen.zsh"
  zgen pmodule marzocchi/zsh-notify
fi

export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS="--color bw"
export FZF_DEFAULT_COMMAND="ag --hidden --ignore .git -g ''"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
