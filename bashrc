# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR=vim
export TERM=st-256color

alias v='vim'

alias ls='ls --color=auto'
alias l='ls -l'
alias ll='ls -l'
alias la='ls -la'
alias rm='rm -i'
alias z='tar -zcvf'
alias uz='tar -zxvf'

alias ga='git add'
alias gb='git branch'
alias gc='git commit -am'
alias gch='git checkout'
alias gd='git diff'
alias gi='git init'
alias gl='git log'
alias gm='git merge --no-ff'
alias gp='git pull'
alias gpom='git pull --rebase origin master'
alias gps='git push'
alias gpsom='git push origin master'
alias gr='git rebase'
alias gri='git rebase -i'
alias gs='git status'

note() {
  ts=$(date +"%s")
  entry="/tmp/entry-$ts.md"
  display_ts="[$(date +"%F %a %R")]"
  echo "# " > $entry
  $EDITOR -c "startinsert!" $entry
  file=${1:-notes.md}
  title=$(head -n1 $entry)
  content=$(tail -n+2 $entry)
  echo -e "$title\n$display_ts\n$content\n" >> $file
}


dpath='$HOME/dcn'
alias d="cd $dpath"
rpath="$dpath/remind"
alias i="echo $1 >> $dpath/in.otl"
alias in="$EDITOR $dpath/in.otl"
alias n="$EDITOR $dpath/next.otl"
alias j="note $dpath/journal.md"
alias nn="note $dpath/notes.md"
alias oj="$EDITOR $dpath/journal.md"
alias on="$EDITOR $dpath/notes.md"
alias nr="note $dpath/recipes.md"
alias or="$EDITOR $dpath/recipes.md"
alias r="remind -c+1 -m $rpath/dcn.rem"
alias rr="remind -c+5 -m $rpath/dcn.rem"
alias ry="remind -c12 -m $rpath/dcn.rem"
alias rd="remind -c+1 -m $rpath/defer.rem"
alias rrd="remind -c+5 -m $rpath/defer.rem"
alias ryd="remind -c+12 -m $rpath/defer.rem"

alias td='tmux attach-session -t dcn || tmux new-session -s dcn'
alias tl='tmux ls'

alias copy='xclip -selection clipboard'

export GOPATH=~/drive/go/
export PATH=$PATH:$GOPATH/bin
alias g='cd $GOPATH'
