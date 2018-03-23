# load direnv, if you are using it

if brew_contains_element "direnv"; then
  function __direnv () {
    eval "$(direnv export bash)"
  }

  if ! contains_element __direnv "${cd_functions[@]}"; then
    cd_functions+=(__direnv)
  fi

  if ! contains_element __direnv "${pushd_functions[@]}"; then
    pushd_functions+=(__direnv)
  fi

  if ! contains_element __direnv "${popd_functions[@]}"; then
    popd_functions+=(__direnv)
  fi
fi
