# load nvm, if you are using it

if brew_contains_element "nvm"; then

  # export NVM_TARGET="$(brew --prefix nvm)"
  if [ -z "$NVM_TARGET" ]; then
    export NVM_TARGET="${BREW_HOME}/opt/nvm"
  fi

  if [ -z "$NVM_DIR" ]; then
    export NVM_DIR="$(realpath "${BREW_HOME}/nvm")"
  fi

  if [ -n "$NVM_VERSION" ] && [ -n "$NVM_DIR" ]; then
    ESCAPED_NVM_VERSION=$(echo "${NVM_VERSION//./$'\.'}")

    PATH=$(path_strip "$PATH" "\./bin")
    PATH=$(path_strip "$PATH" "\./node_modules/\.bin")
    PATH=$(path_strip "$PATH" "${NVM_DIR}/versions/node/v${ESCAPED_NVM_VERSION}/bin")

    PATH="${NVM_DIR}/versions/node/v${NVM_VERSION}/bin:$PATH"
    PATH="./node_modules/.bin:$PATH"
    PATH="./bin:$PATH"
  fi

  # lazy load NVM into a shell session *as a function*
  nvm() {
    echo "Lazy loading nvm..."

    [ -s "$NVM_TARGET/nvm.sh" ] && . "$NVM_TARGET/nvm.sh"

    nvm "$@"
  }

fi
