export EDITOR=vim

alias v='vim'

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
  display_ts="[$(date -jf "%s" $ts +"%Y-%m-%d %a %H:%M")]"
  echo "# " > $entry
  vim -c "startinsert!" $entry
  file=${1:-notes.md}
  title=$(head -n1 $entry)
  content=$(tail -n+2 $entry)
  echo -e "$title\n$display_ts\n$content\n" >> $file
}

dpath='~/dcn'
alias i="echo $1 >> $dpath/in.otl"
alias in="vim $dpath/in.otl"
alias n="vim $dpath/next.otl"

alias td='tmux attach-session -t dcn || tmux new-session -s dcn'
alias tl='tmux ls'

alias chr='open -a "Google Chrome"'
alias copy='xclip -selection clipboard'

export TERM=xterm-16color

export PATH=/usr/local/bin:$PATH
export GOPATH=~/drive/go/
export PATH=$PATH:$GOPATH/bin