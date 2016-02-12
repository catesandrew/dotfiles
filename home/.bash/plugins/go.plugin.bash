# 'load go, if you are using it'

if brew_contains_element "go"; then
    # export GOROOT=$(brew --prefix go)
    export GOROOT="$__dot_brew_home/opt/go"
    export GO_PATH="$__dot_brew_home/go"
fi
