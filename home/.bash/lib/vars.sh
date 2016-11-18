#!/bin/bash

# system type, darwin, linux, ...
__dot_system_type=$(uname)
add_on_exit __dot_system_type

# homebrew home
BREW_HOME=""

# arrays of installed packages, apps, and taps using homebrew.
declare -a __dot_brew_list="()"
add_on_exit __dot_brew_list

declare -a __dot_brew_taps="()"
add_on_exit __dot_brew_taps

declare -a __dot_cask_list="()"
add_on_exit __dot_cask_list
