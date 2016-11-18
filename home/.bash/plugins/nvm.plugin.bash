# load nvm, if you are using it

if brew_contains_element "nvm"; then

  # export NVM_TARGET="$(brew --prefix nvm)"
  if [ -z "$NVM_TARGET" ]; then
    export NVM_TARGET="${BREW_HOME}/opt/nvm"
  fi

  if [ -z "$NVM_DIR" ]; then
    export NVM_DIR="${BREW_HOME}/nvm"
  fi

  # lazy load NVM into a shell session *as a function*
  nvm() {
    echo "Lazy loading nvm..."

    [ -s "$NVM_TARGET/nvm.sh" ] && . "$NVM_TARGET/nvm.sh"

    nvm "$@"
  }

fi
