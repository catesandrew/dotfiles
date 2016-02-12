#!/bin/bash

# Test whether we're in a git repo
is_git_repo() {
    $(git rev-parse --is-inside-work-tree &> /dev/null)
}
add_on_exit is_git_repo

#
# Checks if an element is present in an array.
#
# @param The element to check if present
# @param the array to check in
# @return 0 if present 1 otherwise
contains_element() {
  # http://stackoverflow.com/questions/3685970
  local e
  for e in "${@:2}"; do [[ "$e" == "$1" ]] && return 0; done
  return 1
}
add_on_exit contains_element

#
# Add directory paths to end of PATH. From fink's init.sh
# ex: append_path /home/rmt/bin
path_append() {
  if ! eval test -z "\"\${PATH##*:$1:*}\"" -o -z "\"\${PATH%%*:$1}\"" -o -z "\"\${PATH##$1:*}\"" -o -z "\"\${PATH##$1}\"" ; then
    eval "PATH=\$PATH:$1"
  fi
}
add_on_exit path_append

#
# Add to front of path. From fink's init.sh
# ex: prepend_path /home/rmt/bin
path_prepend() {
  if ! eval test -z "\"\${PATH##*:$1:*}\"" -o -z "\"\${PATH%%*:$1}\"" -o -z "\"\${PATH##$1:*}\"" -o -z "\"\${PATH##$1}\"" ; then
    eval "PATH=$1:\$PATH"
  fi
}
add_on_exit path_prepend

path_strip() {
  echo "$1" | command sed -e "s#$2[^:]*:##g"
}
add_on_exit path_strip

#
# prevent duplicate directories in you PATH variable
#
# @example
# path_munge /path/to/dir is equivalent to PATH=/path/to/dir:$PATH
# path_munge /path/to/dir after is equivalent to PATH=$PATH:/path/to/dir
#
path_munge() {
  if ! [[ $PATH =~ (^|:)$1($|:) ]] ; then
    if [ "$2" = "after" ] ; then
      export PATH=$PATH:$1
    else
      export PATH=$1:$PATH
    fi
  fi
}
add_on_exit path_munge

# Checks if an item is installed in homebrew.
#
# @param The element to check if present
# @return 0 if present 1 otherwise
brew_contains_element() {
  # http://stackoverflow.com/questions/3685970
  local e && e=$(contains_element "$1" "${__dot_brew_list[@]}")
  return $e
}
add_on_exit brew_contains_element

brew_contains_tap() {
  local e && e=$(contains_element "$1" "${__dot_brew_taps[@]}")
  return $e
}
add_on_exit brew_contains_tap

cask_contains_element() {
  local e && e=$(contains_element "$1" "${__dot_cask_list[@]}")
  return $e
}
add_on_exit cask_contains_element
