if ! [ -d "$BREW_HOME" ]; then
    if hash brew 2>/dev/null; then
        BREW_HOME="`brew --prefix`"
        export BREW_HOME
        launchctl setenv BREW_HOME "$BREW_HOME"
    fi
fi

if ! [ -d "$NVM_DIR" ]; then
  if [ -d /usr/local/nvm ]; then
    NVM_DIR=/usr/local/nvm
  elif [ -d "$HOME" ]; then
    NVM_DIR="$HOME"/.nvm
  fi

  export NVM_DIR
  launchctl setenv NVM_DIR "$NVM_DIR"
fi

if [ -d "$NVM_DIR" ]; then
    if [ -f "${NVM_DIR}/alias/default" ]; then
        NVM_VERSION=`cat ${NVM_DIR}/alias/default`
        PATH="./node_modules/.bin:${NVM_DIR}/versions/node/v${NVM_VERSION}/bin:${PATH}"
        NVM_BIN="${NVM_DIR}/versions/node/v${NVM_VERSION}/bin"
        NVM_PATH="${NVM_DIR}/versions/node/v${NVM_VERSION}/lib/node"
        export NVM_BIN
        export NVM_PATH
        launchctl setenv NVM_VERSION "$NVM_VERSION"
        launchctl setenv NVM_BIN "$NVM_BIN"
        launchctl setenv NVM_PATH "$NVM_PATH"
    fi
fi

export PATH
launchctl setenv PATH "$PATH"
