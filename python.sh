set -e
black --line-length 79 $1 > /dev/null 2>&1
flake8 $1
