set -e
goimports -w $1
gofmt -w $1
golint $1
