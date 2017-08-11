if brew_contains_element "lerna" || \
    hash lerna 2>/dev/null; then

  alias lab='lerna bootstrap'
  alias lac='lerna clean'
  alias lad='lerna diff'
  alias lau='lerna updated'
  alias lals='lerna ls'
fi
