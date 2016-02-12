# Load rbebv, if you are using it

if brew_contains_element "rbenv" || \
    hash rbenv 2>/dev/null; then

  RBENV_ROOT="$HOME/.rbenv"
  export RBENV_ROOT
  export RBENV_SHELL=bash

  if brew_contains_element "rbenv"; then
    RBENV_HOME="${__dot_brew_home}/opt/rbenv"
    export RBENV_HOME
  fi

  # Instead of `eval $(rbenv init -)`, lets run it directly here.
  # eval "$(rbenv init -)"
  path_munge "${RBENV_ROOT}/shims" "after"

  # lazy load rbenv
  rbenv() {
    echo "Lazy loading rbenv..."
    command rbenv rehash 2>/dev/null

    rbenv() {
      local command
      command="$1"
      if [ "$#" -gt 0 ]; then
        shift
      fi

      case "$command" in
        rehash|shell)
          eval "$(rbenv "sh-$command" "$@")";;
        *)
          command rbenv "$command" "$@";;
      esac
    }

    if [ -n "${RBENV_HOME}" ]; then
      [[ -e ${RBENV_HOME}/completions/rbenv.bash ]] && \
        . "${RBENV_HOME}/completions/rbenv.bash"
    fi

    rbenv "$@"
  }

fi
