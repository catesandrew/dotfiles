if which brew >/dev/null 2>&1; then
  if [ -f ${BREW_HOME}/etc/bash_completion ]; then
    . ${BREW_HOME}/etc/bash_completion
  fi

  if [ -f ${BREW_HOME}/Library/Contributions/brew_bash_completion.sh ]; then
    . ${BREW_HOME}/Library/Contributions/brew_bash_completion.sh
  fi
fi
