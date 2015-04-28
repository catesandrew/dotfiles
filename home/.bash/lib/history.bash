#!/usr/bin/env bash

# append to bash_history if Terminal.app quits
shopt -s histappend

# history handling
#
# Erase duplicates
# Bash History
export HISTCONTROL="ignoredups"
export HISTCONTROL=erasedups

# Increase the maximum number of commands to remember
# (default is 500)
export HISTSIZE=5000

# Increase the maximum number of lines contained in the history file
# (default is 500)
export HISTFILESIZE=10000

# Make some commands not show up in history
export HISTIGNORE="ls:ls *:cd:cd -:pwd;exit:date:* --help"

export AUTOFEATURE=true autotest

function rh {
  history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head
}
