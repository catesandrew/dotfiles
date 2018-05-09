## load jenv, if you are using it

# 1. install homebrew
# 2. install homebrew jenv
# 3. install homebrew-cask
# 4. install a specific java version using cask (see "homebrew-cask versions"
#    paragraph below)
# 5. add this version for jenv to manage it
# 6. check the version is correctly managed by jenv
# 7. repeat steps 4 to 6 for each version of java you need

# Add the "caskroom/versions" tap to homebrew using:

# brew tap caskroom/versions

# Then you can look at all the versions available:

# brew cask search java

# Then you can install the version(s) you like:

# brew cask install java7
# brew cask install java6

# And add them to be managed by jenv as usual.

# jenv add <javaVersionPathHere>

if brew_contains_element "jenv" || \
    hash jenv 2>/dev/null; then

  export JENV_ROOT="$HOME/.jenv"
  export JENV_SHELL=bash

  if brew_contains_element "jenv"; then
    export JENV_HOME="${BREW_HOME}/opt/jenv"
  fi

  # lazy load jenv
  jenv() {
    echo "Lazy loading jenv..."

    # Instead of `eval $(jenv init -)`, lets run it directly here.
    path_munge "${JENV_ROOT}/shims"

    ${JENV_HOME}/bin/jenv rehash 2>/dev/null
    export JENV_LOADED=1
    # unset JAVA_HOME
    jenv() {
      typeset command
      command="$1"
      if [ "$#" -gt 0 ]; then
        shift
      fi

      case "$command" in
        enable-plugin|rehash|shell|shell-options)
          eval `${JENV_HOME}/bin/jenv "sh-$command" "$@"`;;
        *)
          command ${JENV_HOME}/bin/jenv "$command" "$@";;
      esac
    }

    if [ -n "${JENV_HOME}" ]; then
      [[ -e ${JENV_HOME}/libexec/completions/jenv.bash ]] && \
        . "${JENV_HOME}/libexec/completions/jenv.bash"
    fi

    jenv "$@"
  }
fi
