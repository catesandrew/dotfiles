# load dvm, if you are using it

if brew_contains_element "dvm"; then
  # export DVM_TARGET="$(brew --prefix dvm)"
  if [ -z "$DVM_TARGET" ]; then
    export DVM_TARGET="${BREW_HOME}/opt/dvm"
  fi

  if [ -z "$DVM_DIR" ]; then
    export DVM_DIR="${BREW_HOME}/dvm"
  fi

  # lazy load NVM into a shell session *as a function*
  dvm() {
    echo "Lazy loading dvm..."

    # dvm is a shell function, and must be sourced before it can be used.
    [ -s "$DVM_TARGET/dvm.sh" ] && . "$DVM_TARGET/dvm.sh"

    dvm "$@"
  }
fi
