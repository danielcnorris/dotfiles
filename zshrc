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

alias gi="git init"
alias gs="git status"
alias ga="git add"
alias gb="git branch"
alias gc="git commit -am"
alias gch="git checkout"
alias gcb="git checkout -b"
alias gd="git diff"
alias gl="git log"
alias gm="git merge"
alias gp="git pull"
alias gpom="git pull --rebase origin master"
alias gps="git push"
alias gpsom="git push origin master"
alias gua="git remote add upstream"
alias gpu="git pull upstream master"
alias gf="git fetch"
alias gfa="git fetch --all"
gbt() {
  git fetch
  git branch --track $1 origin/$1
}

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

# I use Google Drive on my Mac at work.
[[ $(uname) = "Linux" ]] && export $dpath="$HOME/Dropbox" || export $dpath="$HOME/Google\ Drive/dcn"
alias n="$EDITOR $dpath/next.otl"
alias j="note $dpath/journal.md"
alias nn="note $dpath/notes.md"
alias oj="$EDITOR $dpath/journal.md"

export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:~/.local/bin

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
