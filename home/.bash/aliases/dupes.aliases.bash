# Some aliases for Homebrew Dupes

if [ "${__dot_system_type}" == "Darwin" ]; then
  if brew_contains_element "ccat"; then
     alias cat='ccat'
  fi

  if brew_contains_element "coreutils" && \
      [ -f "${HOME}/.dircolors" ]; then

    eval $(gdircolors -b "${HOME}/.dircolors")
    alias ls="gls -G --color=always"

    # Directory
    alias md='gmkdir -p'             # Create parent directories on demand
    alias mkdir='gmkdir -pv'         # Create parent directories on demand
    alias rd='grmdir'

    # Add safety nets

    # do not delete / or prompt if deleting more than 3 files at a time
    alias rm='grm -I --preserve-root'

    # confirmation
    alias mv='gmv -i'
    alias cp='gcp -i'
    alias ln='gln -i'

    # Parenting changing perms on /
    alias chown='gchown --preserve-root'
    alias chmod='gchmod --preserve-root'
    alias chgrp='gchgrp --preserve-root'

    if brew_contains_element "grep"; then
      alias shuf=gshuf
      alias grep='ggrep --color=always'
      alias grepno="ggrep --color=never -n -E '.*'"
      alias egrep='gegrep --color=always'
      alias fgrep='gfgrep --color=auto'
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
