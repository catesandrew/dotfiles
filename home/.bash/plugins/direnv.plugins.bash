# load direnv, if you are using it

if brew_contains_element "direnv"; then
  function __direnv () {
    eval "$(direnv export bash)"
  }
fi
