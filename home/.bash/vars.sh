#!/bin/bash

# system type, darwin, linux, ...
__dot_system_type=$(uname)

# homebrew home
__dot_brew_home=""

# arrays of installed packages, apps, and taps using homebrew.
declare -a __dot_brew_list="()"
declare -a __dot_brew_taps="()"
declare -a __dot_cask_list="()"
