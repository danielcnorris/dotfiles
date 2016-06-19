if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

eval $(gpg-agent --daemon)
eval $(ssh-agent -s)
