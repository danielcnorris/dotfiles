stty erase ^?

export EDITOR=vim

alias c='cd'
alias ..='cd ..'
alias ...='cd ../..'

alias l='ls -l'
alias ll='ls -l'
alias la='ls -la'
alias rm='rm -i'

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

alias d='cd ~/drive/dcn'

alias a='cd ~/projects/aslan'
dpath='~/drive/dcn'
rpath="$dpath/remind"
alias i="vim $dpath/in.otl"
alias j="note $dpath/journal.md"
alias nn="note $dpath/notes.md"
alias oj="vim $dpath/journal.md"
alias on="vim $dpath/notes.md"
alias r="remind -c+1 $rpath/dcn.rem"
alias rr="remind -c $rpath/dcn.rem"
alias ry="remind -c+12 $rpath/dcn.rem"
alias rd="remind -c+1 $rpath/defer.rem"
alias rrd="remind -c $rpath/defer.rem"
alias rry="remind -c+12 $rpath/defer.rem"

alias chr='open -a "Google Chrome"'
alias copy='xclip -selection clipboard'

export TERM=xterm-16color

export PATH=~/.cabal/bin:$PATH
export PATH=~/Library/Haskell/bin:$PATH
export PATH=/usr/local/bin:$PATH

eval $(gpg-agent --daemon)
