# ripgrep

if brew_contains_element "rg" && \
  [ -z "$INSIDE_EMACS" ]; then

  export RIPGREP_CONFIG_PATH="${HOME}/.ripgreprc"
  export RIPGREP_TARGET="${BREW_HOME}/opt/ripgrep"

  [[ $- == *i* ]] && . "${RIPGREP_TARGET}/etc/bash_completion.d/rg.bash" 2> /dev/null
fi
