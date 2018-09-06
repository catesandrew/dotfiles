#!/bin/bash

# Append to the Bash history file, rather than overwriting it
shopt -s histappend

# store multi-line commands as a single line
shopt -s cmdhist

# To remove duplicates
# /usr/bin/nl ~/.persistent_history | sort -k 2  -k 1,1nr| uniq -f 1 | ^Crt -n | cut -f 2 > unduped_history
log_bash_persistent_history()
{
  [[ $(history 1) =~ ^\ *[0-9]+\ +([^\ ]+\ [^\ ]+)\ +(.*)$ ]]
  local epoch_part
  local hist_part

  if [ "${__dot_system_type}" == "Darwin" ]; then
    if brew_contains_element "coreutils" || \
        hash gdate 2>/dev/null; then
            epoch_part="$(gdate --date="${BASH_REMATCH[1]}" +%s)"
    else
        epoch_part="$(\date -j -f "%Y-%m-%d %H:%M:%S" "${BASH_REMATCH[1]}" "+%s")"
    fi
  else
      epoch_part="$(date --date="${BASH_REMATCH[1]}" +%s)"
  fi
  local command_part="${BASH_REMATCH[2]}"
  command_part="$(echo -e "${command_part}" | sed -e 's/[[:space:]]*$//')"
  if [ "$command_part" != "$PERSISTENT_HISTORY_LAST" ]; then
    hist_part="$(printf "#%s\n%s" "$epoch_part" "$command_part")"
    echo "$hist_part" >> ~/.persistent_history
    export PERSISTENT_HISTORY_LAST="$command_part"
  fi
}

# history handling
#
# Erase duplicates
# Make repeated commands not show up in history.
# Make commands preceeded by a space not show up in history.
HISTCONTROL=ignoreboth:erasedups

# Eternal bash history.
# ---------------------
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
HISTFILESIZE=
HISTSIZE=

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
HISTTIMEFORMAT="%F %T "

# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history

export AUTOFEATURE=true autotest

function rh {
  history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head
}

# Create a histogram or frequency list of most popular commands used in bash session
# http://stackoverflow.com/questions/18301908
function rh2 {
  history | awk '($2 ~ /^[[:alnum:]]+$/) { ++a[$2]; t = length($2); if (t > l) l = t; } END { for (i in a) printf("%s%" (l - length(i) + 1) "s%5.2f%%\n", i, " ", (a[i] * 100 / NR)); }'
}
