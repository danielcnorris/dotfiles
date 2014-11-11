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
alias gd='git diff'
alias gs='git status'
alias gb='git branch'
alias gp='git pull'
alias gps='git push'
alias gc='git commit'
alias gch='git checkout'
export PATH=$PATH:~/AWS-ElasticBeanstalk-CLI-2.6.3/eb/macosx/python2.7
export PATH=~/.cabal/bin:$PATH
export PATH=~/Library/Haskell/bin:$PATH
export PATH=/usr/local/bin:$PATH
