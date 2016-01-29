stty erase ^?

export EDITOR=vim

alias ..='cd ..'
alias ...='cd ../..'

alias l='ls -l'
alias ll='ls -l'
alias la='ls -la'
alias rm='rm -i'

alias d='cd ~/drive/dcn'
alias a='cd ~/projects/aslan'
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

alias j='note ~/drive/dcn/journal.md'
alias nn='note ~/drive/dcn/notes.md'
alias oj='vim ~/drive/dcn/journal.md'
alias on='vim ~/drive/dcn/notes.md'
alias r='remind -c+1 ~/drive/dcn/remind/dcn.rem'
alias rt='remind ~/drive/dcn/remind/dcn.rem'
alias rmo='remind -c ~/drive/dcn/remind/dcn.rem'
alias ry='remind -c+12 ~/drive/dcn/remind/dcn.rem'
alias rd='remind -c+1 ~/drive/dcn/remind/defer.rem'
alias rdm='remind -c ~/drive/dcn/remind/defer.rem'
alias chr='open -a "Google Chrome"'

alias copy='xclip -selection clipboard'

export TERM=xterm-16color

export PATH=~/.cabal/bin:$PATH
export PATH=~/Library/Haskell/bin:$PATH
export PATH=/usr/local/bin:$PATH

eval $(gpg-agent --daemon)
