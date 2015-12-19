#!/usr/bin/env bash

# append to bash_history if Terminal.app quits
shopt -s histappend

# history handling
#
# Erase duplicates
# Make repeated commands not show up in history.
# Make commands preceeded by a space not show up in history.
HISTCONTROL=ignoreboth:erasedups

# Increase the maximum number of commands to remember
# (default is 500)
HISTSIZE=10000

# Increase the maximum number of lines contained in the history file
# (default is 500)
HISTFILESIZE=200000

# make single-word commands not show up in history, along with anything dash or double dash help
HISTIGNORE="npm ls:cd -:mvim .:em .:* --+(h|he|hel|help):* -+(h|he|hel|help):+([-%+.0-9@A-Z_a-z])"

export AUTOFEATURE=true autotest

function rh {
  history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head
}

# Create a histogram or frequency list of most popular commands used in bash session
# http://stackoverflow.com/questions/18301908
function rh2 {
  history | awk '($2 ~ /^[[:alnum:]]+$/) { ++a[$2]; t = length($2); if (t > l) l = t; } END { for (i in a) printf("%s%" (l - length(i) + 1) "s%5.2f%%\n", i, " ", (a[i] * 100 / NR)); }'
}
