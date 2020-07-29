## load jenv, if you are using it
if brew_contains_element "jenv" || \
    hash jenv 2>/dev/null; then

  if [ -z "$JENV_ROOT" ]; then
    export JENV_ROOT="$HOME/.jenv"
  fi

  if [ -z "$JENV_HOME" ]; then
    export JENV_HOME="${BREW_HOME}/opt/jenv"
  fi

  jenv() {
    if [ "$JENV_LOADED" = "1" ]; then
      echo "Jenv is correctly loaded..."
      jenv() {
        typeset command
        command="$1"
        if [ "$#" -gt 0 ]; then
          shift
        fi

        case "$command" in
          enable-plugin|rehash|shell|shell-options)
            eval `${JENV_HOME}/bin/jenv "sh-$command" "$@"`;;
          *)
            command ${JENV_HOME}/bin/jenv "$command" "$@";;
        esac
      }
    else
      # Instead of `eval $(jenv init -)`, lets run it directly here.
      echo "Lazy loading jenv..."

      # Strip other version from PATH
      export PATH=$(path_strip "$PATH" "${JENV_ROOT}/shims")
      path_munge "${JENV_ROOT}/shims"

      export JENV_SHELL=bash
      export JENV_LOADED=1
      unset JAVA_HOME

      jenv() {
        typeset command
        command="$1"
        if [ "$#" -gt 0 ]; then
          shift
        fi

        case "$command" in
          enable-plugin|rehash|shell|shell-options)
            eval `${JENV_HOME}/bin/jenv "sh-$command" "$@"`;;
          *)
            command ${JENV_HOME}/bin/jenv "$command" "$@";;
        esac
      }

      if [ -n "${JENV_HOME}" ]; then
        [[ -e ${JENV_HOME}/libexec/completions/jenv.bash ]] && \
          source "${JENV_HOME}/libexec/completions/jenv.bash"
      fi

      ${JENV_HOME}/bin/jenv rehash 2>/dev/null

      if [ -n "${JENV_HOME}" ]; then
        [[ -e ${JENV_ROOT}/plugins/export/etc/jenv.d/init/export_jenv_hook.bash ]] && \
          source "${JENV_ROOT}/plugins/export/etc/jenv.d/init/export_jenv_hook.bash"
      fi
    fi

    jenv "$@"
  }
fi
