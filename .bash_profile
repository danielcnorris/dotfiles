if [ -f ~/.bashrc ]
then
    source ~/.bashrc
fi

alias ..='cd ..'
alias ...='cd ../..'

alias ll='ls -l'
alias la='ls -la'
alias rm='rm -i'

alias chr='open -a "Google Chrome"'

export ds=~/Desktop
alias cds='cd $ds'
alias lds='ll $ds'

alias ga='git add'
alias gb='git branch'
alias gc='git commit -am'
alias gch='git checkout'
alias gd='git diff'
alias gl='git log'
alias gm='git merge --no-ff'
alias gp='git pull'
alias gpom='git pull --rebase origin master'
alias gps='git push'
alias gpsom='git push origin master'
alias gr='git rebase'
alias gri='git rebase -i'
alias gs='git status'

export PATH=$PATH:~/AWS-ElasticBeanstalk-CLI-2.6.3/eb/macosx/python2.7
export PATH=~/.cabal/bin:$PATH
export PATH=~/Library/Haskell/bin:$PATH
export PATH=/usr/local/bin:$PATH
