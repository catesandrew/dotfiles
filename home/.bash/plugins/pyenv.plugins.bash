 # load pyenv, if you are using it

if brew_contains_element "pyenv" || \
    hash pyenv 2>/dev/null; then

  if [ -z "$PYENV_ROOT" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
  fi

  if [ -z "$PYENV_HOME" ]; then
    export PYENV_HOME="${BREW_HOME}/opt/pyenv"
  fi

  if [ -n "$PYENV_ROOT" ]; then
    PATH=$(path_strip "$PATH" "${PYENV_ROOT}/shims")
    PATH="${PYENV_ROOT}/shims:$PATH"
  fi

  export PYENV_SHELL=$SHELL

  if brew_contains_element "pyenv"; then
    export PYENV_HOME="${BREW_HOME}/opt/pyenv"
  fi

  HAS_VIRTUALENV=0
  if brew_contains_element "pyenv-virtualenv" || \
      hash pyenv-virtualenv-init 2>/dev/null; then
    HAS_VIRTUALENV=1
  fi

  # lazy load pyenv
  pyenv() {
    echo "Lazy loading pyenv..."

    # Instead of `eval $(pyenv init -)`, lets run it directly here.
    # eval "$(pyenv init -)"
    path_munge "${PYENV_ROOT}/shims"

    command pyenv rehash 2>/dev/null
    pyenv() {
      local command
      command="$1"
      if [ "$#" -gt 0 ]; then
        shift
      fi

      case "$command" in
        rehash|shell)
          eval "$(pyenv "sh-$command" "$@")";;
        *)
          command pyenv "$command" "$@";;
      esac
    }

    if [ -n "${PYENV_HOME}" ]; then
      [[ -e ${PYENV_HOME}/completions/pyenv.bash ]] && \
        . "${PYENV_HOME}/completions/pyenv.bash"
    fi

    if [ $HAS_VIRTUALENV ]; then
      echo "Lazy loading virtualenv..."
      eval "$(pyenv virtualenv-init -)"
      unset HAS_VIRTUALENV
    else
      unset HAS_VIRTUALENV
    fi

    pyenv "$@"
  }
fi
