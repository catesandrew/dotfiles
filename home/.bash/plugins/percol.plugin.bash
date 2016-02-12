# Search & Select history and fasd with percol

# Notice
## You have to upgrade bash to bash 4.x on Mac OS X.
## http://stackoverflow.com/questions/16416195

# Usage
## C-r to search & select from history
## zzz to search & select from fasd
if hash percol 2>/dev/null && \
    [ "${BASH_VERSINFO[0]}" -gt 3 ]; then

  __replace_by_history() {
    if command -v tac>/dev/null; then
      alias _tac=tac
    else
      alias _tac="tail -r"
    fi
    local l=$(HISTTIMEFORMAT= history | _tac | sed -e 's/^\ *[0-9]*\ *//' | percol --query "$READLINE_LINE")
    READLINE_LINE="$l"
    READLINE_POINT=${#l}
  }
  # bind -x '"\C-r": __replace_by_history'

  # bind zzz to percol if fasd enable
  if brew_contains_element "fasd" || \
      hash fasd 2>/dev/null; then

    zzz(){
      local l=$(fasd -d | awk '{print $2}' | percol)
      cd $l
    }
  fi
fi
