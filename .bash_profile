if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

alias ll='ls -l'
alias la='ls -la'
alias rm='rm -i'
alias chr='open -a "Google Chrome"'

export PATH=$PATH:~/AWS-ElasticBeanstalk-CLI-2.6.3/eb/macosx/python2.7
export PATH=~/.cabal/bin:$PATH
export PATH=~/Library/Haskell/bin:$PATH
export PATH=/usr/local/bin:$PATH
