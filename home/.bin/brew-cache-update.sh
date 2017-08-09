#!/bin/bash
function add_on_exit() {
  true
}

. "${BASH_IT}/lib/funcs.sh"

OUTPUT=${HOME}/.brew-init-bash
s=""

BREW_HOME="$(brew --prefix)"
echo "BREW_HOME=${BREW_HOME}" > "${OUTPUT}"
echo "export BREW_HOME" >> "${OUTPUT}"

__dot_brew_list=($(brew list | sed 's/:.*//'))
__serialise_to s "${__dot_brew_list[@]}"

echo "__dot_brew_list=\"$s\"" >> "${OUTPUT}"
echo "export __dot_brew_list" >> "${OUTPUT}"

__dot_brew_taps=($(brew tap | sed 's/:.*//'))
__serialise_to s "${__dot_brew_taps[@]}"

echo "__dot_brew_taps=\"$s\"" >> "${OUTPUT}"
echo "export __dot_brew_taps" >> "${OUTPUT}"

__dot_cask_list=($(brew cask list | sed 's/:.*//'))
__serialise_to s "${__dot_cask_list[@]}"

echo "__dot_cask_list=\"$s\"" >> "${OUTPUT}"
echo "export __dot_cask_list" >> "${OUTPUT}"

unset s
unset OUTPUT
