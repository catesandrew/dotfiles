#!/bin/bash

print_status() {
  echo
  echo "## $1"
  echo
}

bail() {
  echo 'Error executing command, exiting'
  exit 1
}

exec_cmd_nobail() {
  echo "+ $1"
  bash -c "$1"
}

exec_cmd() {
  exec_cmd_nobail "$1" || bail
}

exec_sudo_cmd_nobail() {
  echo "+ $1"
  sudo bash -c "$1"
}

exec_sudo_cmd() {
  exec_sudo_cmd_nobail "$1" || bail
}

# Header logging
e_header() {
    printf "\n$(tput setaf 7)%s$(tput sgr0)\n" "$@"
}

# Success logging
e_success() {
    printf "$(tput setaf 64)✓ %s$(tput sgr0)\n" "$@"
}

# Error logging
e_error() {
    printf "$(tput setaf 1)x %s$(tput sgr0)\n" "$@"
}

# Warning logging
e_warning() {
    printf "$(tput setaf 136)! %s$(tput sgr0)\n" "$@"
}

# Ask for confirmation before proceeding
seek_confirmation() {
    printf "\n"
    e_warning "$@"
    read -p "Continue? (y/n) " -n 1
    printf "\n"
}

# Test whether the result of an 'ask' is a confirmation
is_confirmed() {
    if [[ "$REPLY" =~ ^[Yy]$ ]]; then
      return 0
    fi
    return 1
}

# Test whether we're in a git repo
is_git_repo() {
    $(git rev-parse --is-inside-work-tree &> /dev/null)
}

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

#
# Add directory paths to end of PATH. From fink's init.sh
# ex: append_path /home/rmt/bin
path_append() {
  if ! eval test -z "\"\${PATH##*:$1:*}\"" -o -z "\"\${PATH%%*:$1}\"" -o -z "\"\${PATH##$1:*}\"" -o -z "\"\${PATH##$1}\"" ; then
    eval "PATH=\$PATH:$1"
  fi
}

#
# Add to front of path. From fink's init.sh
# ex: prepend_path /home/rmt/bin
path_prepend() {
  if ! eval test -z "\"\${PATH##*:$1:*}\"" -o -z "\"\${PATH%%*:$1}\"" -o -z "\"\${PATH##$1:*}\"" -o -z "\"\${PATH##$1}\"" ; then
    eval "PATH=$1:\$PATH"
  fi
}

#
# prevent duplicate directories in you PATH variable
#
# @example
# pathmunge /path/to/dir is equivalent to PATH=/path/to/dir:$PATH
# pathmunge /path/to/dir after is equivalent to PATH=$PATH:/path/to/dir
#
# if ! type pathmunge > /dev/null 2>&1
# then
function pathmunge () {
  if ! [[ $PATH =~ (^|:)$1($|:) ]] ; then
    if [ "$2" = "after" ] ; then
      export PATH=$PATH:$1
    else
      export PATH=$1:$PATH
    fi
  fi
}
# fi
