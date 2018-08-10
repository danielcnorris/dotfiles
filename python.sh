set -e
# black -q --line-length 79 $1
yapf -i $1
flake8 $1
