#!/bin/bash
function add_on_exit() {
  true
}

. "${BASH_IT}/lib/funcs.sh"

if hash brew 2>/dev/null; then
  if [ -f ${HOME}/.brew-init-bash ]; then
    OUTPUT=${HOME}/.brew-init-bash
  else
    OUTPUT=""
  fi

  s=""

  BREW_HOME="$(brew --prefix)"
  echo "BREW_HOME=${BREW_HOME}" >| "${OUTPUT}"
  echo "export BREW_HOME" >> "${OUTPUT}"
  echo "HOMEBREW_PREFIX=${BREW_HOME}" >> "${OUTPUT}"
  echo "export HOMEBREW_PREFIX" >> "${OUTPUT}"

  HOMEBREW_CELLAR="$(brew --cellar)"
  echo "HOMEBREW_CELLAR=${HOMEBREW_CELLAR}" >> "${OUTPUT}"
  echo "export HOMEBREW_CELLAR" >> "${OUTPUT}"

  HOMEBREW_REPOSITORY="$(brew --repository)"
  echo "HOMEBREW_REPOSITORY=${HOMEBREW_REPOSITORY}" >> "${OUTPUT}"
  echo "export HOMEBREW_REPOSITORY" >> "${OUTPUT}"

  echo "BASH_IT=${BASH_IT}" >> "${OUTPUT}"
  echo "export BASH_IT" >> "${OUTPUT}"

  __dot_brew_list=($(brew list --formula | sed 's/:.*//'))
  __serialise_to s "${__dot_brew_list[@]}"

  echo "__dot_brew_list=\"$s\"" >> "${OUTPUT}"
  echo "export __dot_brew_list" >> "${OUTPUT}"

  __dot_brew_taps=($(brew tap | sed 's/:.*//'))
  __serialise_to s "${__dot_brew_taps[@]}"

  echo "__dot_brew_taps=\"$s\"" >> "${OUTPUT}"
  echo "export __dot_brew_taps" >> "${OUTPUT}"

  __dot_cask_list=($(brew list --formula | sed 's/:.*//'))
  __serialise_to s "${__dot_cask_list[@]}"

  echo "__dot_cask_list=\"$s\"" >> "${OUTPUT}"
  echo "export __dot_cask_list" >> "${OUTPUT}"

  unset s
  unset OUTPUT
fi
