if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

if [[ -z $SSH_AUTH_SOCK ]]; then
  eval $(ssh-agent -s)
  ssh-add ~/.ssh/id_rsa
  env | grep SSH > $HOME/.ssh_agent
  echo "export SSH_AUTH_SOCK" >> .ssh_agent
  echo "export SSH_AGENT_PID" >> .ssh_agent
fi

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
