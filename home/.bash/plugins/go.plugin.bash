# load go, if you are using it

# export GOROOT=$(brew --prefix go)
if brew_contains_element "go" || \
    hash go 2>/dev/null; then
    export GOROOT="${__dot_brew_home}/opt/go/libexec"
    export GOPATH="${__dot_brew_home}/go"
fi
