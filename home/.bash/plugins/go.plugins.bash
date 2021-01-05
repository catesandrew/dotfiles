# load go, if you are using it

# export GOROOT=$(brew --prefix go)
if brew_contains_element "go" || \
    hash go 2>/dev/null; then
  if [ "$__dot_system_type" == "Linux" ]; then
    export GOROOT="${BREW_HOME}/opt/go/libexec"
    export GOPATH="${BREW_HOME}/../go"
  else
    export GOROOT="${BREW_HOME}/opt/go/libexec"
    export GOPATH="${BREW_HOME}/go"

    # Strip other version from PATH
    # export PATH=$(path_strip "$PATH" "${GOPATH}/bin")

    # Append
    path_append "${GOPATH}/bin"
  fi
fi
