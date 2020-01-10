#!/bin/bash

# npm (Node Package Manager) completion
# taken from: eval "$(npm completion)"

#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm
#

_npm_completion () {
  local words cword
  if type _get_comp_words_by_ref &>/dev/null; then
    _get_comp_words_by_ref -n = -n @ -n : -w words -i cword
  else
    cword="$COMP_CWORD"
    words=("${COMP_WORDS[@]}")
  fi

  local si="$IFS"
  IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
                         COMP_LINE="$COMP_LINE" \
                         COMP_POINT="$COMP_POINT" \
                         npm completion -- "${words[@]}" \
                         2>/dev/null)) || return $?
  IFS="$si"
  if type __ltrim_colon_completions &>/dev/null; then
    __ltrim_colon_completions "${words[cword]}"
  fi
}

# has to be delay loaded due to nvm
# complete -o default -F _npm_completion npm
