# Some aliases for Homebrew Dupes

if [ "${__dot_system_type}" == "Darwin" ]; then
  if brew_contains_element "coreutils" && \
      [ -f "${HOME}/.dircolors" ]; then

    eval $(gdircolors -b "${HOME}/.dircolors")
    alias ls="gls -G --color=always"

    if brew_contains_element "grep"; then
      alias shuf=gshuf
      alias grep="ggrep --color=always"
      alias grepno="ggrep --color=never -n -E '.*'"
      alias egrep="gegrep --color=always"
    fi
  else
    # colored grep
    export GREP_COLOR='1;33'

    # colored ls
    export CLICOLOR=1
    export LSCOLORS='Gxfxcxdxdxegedabagacad'

    # compact view
    alias ls='ls -G'
  fi
elif [ "${__dot_system_type}" == "Linux" ]; then
  if [ -f "${HOME}/.dircolors" ]; then
    eval $(dircolors -b "${HOME}/.dircolors")
    alias ls="ls -G --color=always"
    alias egrep="egrep --color=always"
    alias grep="grep --color=always"
    alias grepno="grep --color=never -n -E '.*'"
  fi
fi
