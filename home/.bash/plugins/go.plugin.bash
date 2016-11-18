# load go, if you are using it

# export GOROOT=$(brew --prefix go)
if brew_contains_element "go" || \
    hash go 2>/dev/null; then
    export GOROOT="${BREW_HOME}/opt/go/libexec"
    export GOPATH="${BREW_HOME}/go"
fi
