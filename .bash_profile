stty erase ^?

export EDITOR=vim

alias ..='cd ..'
alias ...='cd ../..'

alias ll='ls -l'
alias la='ls -la'
alias rm='rm -i'

export gd=~/drive

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

source $HOME/.task.sh

note() {
  ts=$(date +"%s")
  entry="/tmp/entry-$ts.md"
  display_ts="[$(date -jf "%s" $ts +"%Y-%m-%d %a %H:%M")]"
  echo "# " > $entry
  vim -c "startinsert!" $entry
  file=${1:-notes.md}
  title=$(head -n1 $entry)
  content=$(tail -n+2 $entry)
  echo -e "$title\n $display_ts\n $content\n" >> $file
}

alias j='note ~/drive/reference/journal.md'
alias nn='note ~/drive/reference/notes.md'
alias oj='vim ~/drive/reference/journal.md'
alias on='vim ~/drive/reference/notes.md'
alias r='remind ~/drive/remind/dcn.rem'
alias rw='remind -c+1 ~/drive/remind/dcn.rem'
alias rm='remind -c ~/drive/remind/dcn.rem'
alias ry='remind -c+12 ~/drive/remind/dcn.rem'
alias chr='open -a "Google Chrome"'

alias copy='xclip -selection clipboard'

alias ioa5="ssh -t bastion 'ssh -t maroon-ioa-di-5'"
alias pc1="ssh -t bastion 'ssh -t maroon-poc-di-1'"

export TERM=xterm-16color

export PATH=~/.cabal/bin:$PATH
export PATH=~/Library/Haskell/bin:$PATH
export PATH=/usr/local/bin:$PATH

function genpass() {
    LC_CTYPE=C tr -dc 'A-Za-z0-9_!@#$$%^&*()|+=' < /dev/urandom | \
        head -c32 | xargs
}

eval $(gpg-agent --daemon)
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
