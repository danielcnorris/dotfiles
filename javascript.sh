set -e
prettier --loglevel warn --write $1
standard --fix $1 2>/dev/null
