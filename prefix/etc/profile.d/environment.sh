#!/bin/bash

if [ ! -d "$BREW_HOME" ]; then
    if hash brew 2>/dev/null; then
        BREW_HOME=$(brew --prefix)
        export BREW_HOME
        # launchctl setenv BREW_HOME "$BREW_HOME"

    fi
fi

if [ -d "${HOME}/.bash" ]; then
    BASH_IT="${HOME}/.bash"
    export BASH_IT
    # launchctl setenv BASH_IT "$BASH_IT"
fi

if ! [ -d "$RBENV_HOME" ]; then
    if [ -d /usr/local/opt/rbenv ]; then
        RBENV_HOME=/usr/local/opt/rbenv
    fi

    export RBENV_HOME
    # launchctl setenv RBENV_HOME "$RBENV_HOME"
fi

if [ ! -d "$RBENV_ROOT" ]; then
    if [ -d "${HOME}/.rbenv" ]; then
        RBENV_ROOT="${HOME}/.rbenv"
    fi

    export RBENV_ROOT
    # launchctl setenv RBENV_ROOT "$RBENV_ROOT"
fi

if [ -d "$RBENV_ROOT" ]; then
    if [ -f "${RBENV_ROOT}/version" ]; then
        RBENV_VERSION=$(cat "${RBENV_ROOT}/version")

        # PATH=$(path_strip "$PATH" "${RBENV_ROOT}/shims")
        # PATH="${RBENV_ROOT}/shims:$PATH"

        export RBENV_VERSION
        # launchctl setenv RBENV_VERSION "$RBENV_VERSION"
    fi
fi

if [ ! -d "$NVM_DIR" ]; then
    if [ -d /usr/local/nvm ]; then
        NVM_DIR=/usr/local/nvm
    elif [ -d "$HOME" ]; then
        NVM_DIR="$HOME"/.nvm
    fi

    export NVM_DIR
    # launchctl setenv NVM_DIR "$NVM_DIR"
fi

if [ -d "$NVM_DIR" ]; then
    if [ -f "${NVM_DIR}/alias/default" ]; then
        NVM_VERSION=$(cat "${NVM_DIR}/alias/default")
        # ESCAPED_NVM_VERSION=$(echo "${NVM_VERSION//./$'\.'}")
        # PATH="./bin:./node_modules/.bin:${NVM_DIR}/versions/node/v${NVM_VERSION}/bin:${PATH}"

        # PATH=$(path_strip "$PATH" "\./bin")
        # PATH=$(path_strip "$PATH" "\./node_modules/\.bin")
        # PATH=$(path_strip "$PATH" "${NVM_DIR}/versions/node/v${NVM_VERSION}/bin")
        #
        # PATH="${NVM_DIR}/versions/node/v${NVM_VERSION}/bin:$PATH"
        # PATH="./node_modules/.bin:$PATH"
        # PATH="./bin:$PATH"

        NVM_BIN="${NVM_DIR}/versions/node/v${NVM_VERSION}/bin"
        NVM_PATH="${NVM_DIR}/versions/node/v${NVM_VERSION}/lib/node"
        export NPM_VERSION
        export NVM_BIN
        export NVM_PATH
        # launchctl setenv NVM_VERSION "$NVM_VERSION"
        # launchctl setenv NVM_BIN "$NVM_BIN"
        # launchctl setenv NVM_PATH "$NVM_PATH"
    fi
fi

if [ ! -d "$JENV_HOME" ]; then
    if [ -d /usr/local/opt/jenv ]; then
        JENV_HOME=/usr/local/opt/jenv
    elif [ -d "$HOME" ]; then
        JENV_HOME="${HOME}/.jenv"
    fi

    export JENV_HOME
    # launchctl setenv JENV_HOME "$JENV_HOME"
fi

JENV_ROOT="${HOME}/.jenv"
export JENV_ROOT
# launchctl setenv JENV_ROOT "$JENV_ROOT"

if [ -d "$JENV_ROOT" ]; then
    if [ -f "${JENV_ROOT}/version" ]; then
        JENV_VERSION=$(cat "${JENV_ROOT}/version")
        # check to verify version is not `system`
        if [ "$JENV_VERSION" = "system" ]; then
          JAVA_HOME="$(/usr/libexec/java_home)"
        else
          JAVA_HOME="$(/usr/libexec/java_home "-v${JENV_VERSION}")"
        fi
        export JAVA_HOME
        export JENV_VERSION
    fi
fi

if [ -d "${BREW_HOME}/go" ]; then
    GOROOT="${BREW_HOME}/opt/go/libexec"
    GOPATH="${BREW_HOME}/go"

    export GOROOT
    export GOPATH
    # launchctl setenv GOROOT "$GOROOT"
    # launchctl setenv GOPATH "$GOPATH"

    # PATH="${PATH}:${GOPATH}/bin"
fi

# launchctl setenv PATH "$PATH"
