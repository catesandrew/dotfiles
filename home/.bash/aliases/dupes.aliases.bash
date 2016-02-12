# Some aliases for Homebrew Dupes

if [ $__dot_system_type == "Darwin" ]; then
  if brew_contains_tap "homebrew/dupes"; then
    if brew_contains_element "grep"; then
      alias grep='ggrep --color'
      alias grepno="ggrep --color=never -n -E '.*'"
      alias ls='gls --color'
    fi
  fi
fi
