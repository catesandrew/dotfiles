#!/bin/bash

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

strip_path() {
  echo "$1" | command sed -e "s#$2[^:]*:##g"
}


#
# prevent duplicate directories in you PATH variable
#
# @example
# pathmunge /path/to/dir is equivalent to PATH=/path/to/dir:$PATH
# pathmunge /path/to/dir after is equivalent to PATH=$PATH:/path/to/dir
#
function pathmunge () {
  if ! [[ $PATH =~ (^|:)$1($|:) ]] ; then
    if [ "$2" = "after" ] ; then
      export PATH=$PATH:$1
    else
      export PATH=$1:$PATH
    fi
  fi
}
