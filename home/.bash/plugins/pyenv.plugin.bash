 # load pyenv, if you are using it

if brew_contains_element "pyenv" || \
    hash pyenv 2>/dev/null; then

  export PYENV_ROOT="$HOME/.pyenv"
  export PYENV_SHELL=bash

  if brew_contains_element "pyenv"; then
    export PYENV_HOME="${BREW_HOME}/opt/pyenv"
  fi

  # Instead of `eval $(pyenv init -)`, lets run it directly here.
  # eval "$(pyenv init -)"
  path_munge "${PYENV_ROOT}/shims" "after"

  HAS_VIRTUALENV=0
  if brew_contains_element "pyenv-virtualenv" || \
      hash pyenv-virtualenv-init 2>/dev/null; then
    HAS_VIRTUALENV=1
  fi

  # lazy load pyenv
  pyenv() {
    echo "Lazy loading pyenv..."
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
