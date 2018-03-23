# These functions override builtin BASH commands that change directories.
#
# This script should be added to either the system wide shell initialisation
# file (/etc/profile) or a user specific initialisation file (~/.bash_profile or
# ~/.profile). In addition, if you are using X, terminals you start up should be
# login terminals (typically -ls, --ls or something to that effect).
if brew_contains_element "ondir"; then
  function __ondir () {
    eval "$(ondir "$OLDPWD" "$PWD")"
  }

  if ! contains_element __ondir "${cd_functions[@]}"; then
    cd_functions+=(__ondir)
  fi

  if ! contains_element __ondir "${pushd_functions[@]}"; then
    pushd_functions+=(__ondir)
  fi

  if ! contains_element __ondir "${popd_functions[@]}"; then
    popd_functions+=(__ondir)
  fi
fi
