# TODO: Update to brew bash completion...
if which brew >/dev/null 2>&1; then
  if [ -f ${BREW_HOME}/etc/bash_completion ]; then
    . ${BREW_HOME}/etc/bash_completion
  fi

  if [ -f ${BREW_HOME}/Library/Contributions/brew_bash_completion.sh ]; then
    . ${BREW_HOME}/Library/Contributions/brew_bash_completion.sh
  fi
fi

# TODO: Till then use previous bash completion

# $Id: bash_completion.sh,v 1.2 2006/02/25 01:21:33 ianmacd Exp $

# Check for bash (and that we haven't already been sourced).
[ -z "$BASH_VERSION" -o -n "$BASH_COMPLETION" ] && return

# Check for recent enough version of bash.
bash=${BASH_VERSION%.*}; bmajor=${bash%.*}; bminor=${bash#*.}

# Check for interactive shell.
if [ -n "$PS1" ]; then
  if [ $bmajor -eq 2 -a $bminor '>' 04 ] || [ $bmajor -gt 2 ]; then
    if [ -f /usr/local/etc/bash_completion ]; then
      . /usr/local/etc/bash_completion
    fi
  fi
fi
unset bash bminor bmajor
