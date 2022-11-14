if brew_contains_element "cheat" || \
    hash cheat 2>/dev/null; then
  export CHEAT_USE_FZF=true
fi
