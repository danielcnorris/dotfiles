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
alias gpu="git pull upstream"

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
  export DPATH=$HOME/Dropbox 
  alias rd='redshift -O 5500'
  alias rn='redshift -O 2000'
else
  export DPATH="$HOME/Google\ Drive/dcn"
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
fi

alias n="$EDITOR $DPATH/next.otl"
alias j="note $DPATH/journal.md"
alias nn="note $DPATH/notes.md"
alias oj="$EDITOR $DPATH/journal.md"


export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:~/.local/bin

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS='--color bw'
