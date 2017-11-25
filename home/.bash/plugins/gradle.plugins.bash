# load gradle, if you are using it

if brew_contains_element "gradle" || \
    hash gradle 2>/dev/null; then
  true
fi
