stty erase ^?

alias ..='cd ..'
alias ...='cd ../..'

alias em='emacs -nw'

alias ll='ls -l --color'
alias la='ls -la --color'
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

export TERM=xterm-16color

function genpass() {
    LC_CTYPE=C tr -dc 'A-Za-z0-9_!@#$$%^&*()|+=' < /dev/urandom | \
        head -c32 | xargs
}

eval $(gpg-agent --daemon)
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
