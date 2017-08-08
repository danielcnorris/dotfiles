export EDITOR=vim

alias python=python2
alias pip=pip2

alias v='vim'

alias ls='ls -G'
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
  echo "#  " > $entry
  $EDITOR -c "normal $" +startinsert $entry
  file=${1:-notes.md}
  title=$(head -n1 $entry)
  content=$(tail -n+2 $entry)
  echo -e "$title\n$display_ts\n$content\n" >> "$file"
}

wpath="$HOME/Google\ Drive"
dpath="$wpath/dcn"
alias d="cd $dpath"
alias i="echo $1 >> $dpath/in.otl"
alias in="$EDITOR $dpath/in.otl"
alias n="$EDITOR $dpath/next.otl"
alias nn="note $dpath/notes.md"
alias on="$EDITOR $dpath/notes.md"

alias td='tmux attach-session -t dcn || tmux new-session -s dcn'
alias tl='tmux ls'

alias chr='open -a "Google Chrome"'
alias copy='xclip -selection clipboard'

export TERM=xterm-16color

export PATH="/usr/local/opt/python/libexec/bin:$PATH"

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
source /usr/local/bin/virtualenvwrapper.sh

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.bash ] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.bash
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.bash ] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.bash
