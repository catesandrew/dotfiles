if [ "$__dot_system_type" == "Darwin" ] && \
     brew_contains_element "archey"; then
  archey -o
else
  if brew_contains_element "archey" || \
      hash archey 2>/dev/null; then
    archey
  fi
fi
