#!/bin/bash

# Append to the Bash history file, rather than overwriting it
shopt -s histappend

# store multi-line commands as a single line
shopt -s cmdhist

# history handling
#
# Erase duplicates
# Make repeated commands not show up in history.
# Make commands preceeded by a space not show up in history.
HISTCONTROL=ignoreboth:erasedups

# Huge history. Doesn't appear to slow things down, so why not?
# - Increase the maximum number of commands to remember (default is 500)
HISTSIZE=50000000
# - Increase the maximum number of lines contained in the history file (default is 500)
HISTFILESIZE=50000000

# Don't record some commands
# - `npm +(ls|install|view|update)` will not record `npm ls`, `npm install`, etc.
# - `ncu -+(a)` will not record `ncu -a`
# - `* --+(h|he|hel|help)` will not record a single-word command followed by
#   double dash `--h`, `--he`, etc.
# - `* -+(h|he|hel|help)` will not record a single-word command followed by
#   single dash `-h`, `-he`, etc.
# - `+([-%+.0-9@A-Z_a-z])` - the best one by far since it will not record any
#   single-word commands, or basically any command executed without parameters.
HISTIGNORE="npm +(ls|install|view|update):ncu -+(a):cd -:mvim .:em .:* --+(h|he|hel|help):* -+(h|he|hel|help):+([-%+.0-9@A-Z_a-z])"

# Useful timestamp format
HISTTIMEFORMAT='%F %T '

export AUTOFEATURE=true autotest

function rh {
  history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head
}

# Create a histogram or frequency list of most popular commands used in bash session
# http://stackoverflow.com/questions/18301908
function rh2 {
  history | awk '($2 ~ /^[[:alnum:]]+$/) { ++a[$2]; t = length($2); if (t > l) l = t; } END { for (i in a) printf("%s%" (l - length(i) + 1) "s%5.2f%%\n", i, " ", (a[i] * 100 / NR)); }'
}
