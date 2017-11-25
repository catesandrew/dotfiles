#!/bin/bash
if [ "$__dot_system_type" == "Darwin" ] && [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
  if brew_contains_tap "homebrew/command-not-found"; then
    eval "$(brew command-not-found-init)";
  fi
fi
