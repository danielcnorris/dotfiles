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
