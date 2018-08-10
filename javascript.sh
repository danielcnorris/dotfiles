set -e
prettier --write $1
standard --fix $1
