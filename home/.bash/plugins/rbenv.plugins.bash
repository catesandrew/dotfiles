# Load rbenv, if you are using it

if brew_contains_element "rbenv"; then

  if [ -z "$RBENV_ROOT" ]; then
    export RBENV_ROOT="$HOME/.rbenv"
  fi

  if [ -z "$RBENV_HOME" ]; then
    export RBENV_HOME="${BREW_HOME}/opt/jenv"
  fi

  if [ -n "$RBENV_ROOT" ]; then
    PATH=$(path_strip "$PATH" "${RBENV_ROOT}/shims")
    PATH="${RBENV_ROOT}/shims:$PATH"
  fi

  # lazy load rbenv
  rbenv() {

    if [ "$RBENV_LOADED" = "1" ]; then
      echo "rbenv is correctly loaded..."
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
    else
      echo "Lazy loading rbenv..."
      # Instead of `eval $(rbenv init -)`, lets run it directly here.

      # Strip other version from PATH
      export PATH=$(path_strip "$PATH" "${RBENV_ROOT}/shims")
      path_munge "${RBENV_ROOT}/shims"

      export RBENV_SHELL=bash
      export RBENV_LOADED=1

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
          source "${RBENV_HOME}/completions/rbenv.bash"
      fi

      command rbenv rehash 2>/dev/null
    fi

    rbenv "$@"
  }
fi
