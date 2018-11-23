set -e
black -q $1
flake8 $1
