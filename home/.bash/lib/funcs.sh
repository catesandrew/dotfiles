#!/bin/bash

# Test whether we're in a git repo
is_git_repo() {
  if [[ -e "$(git rev-parse --git-dir 2> /dev/null)" ]]; then
    echo 1
  fi
  echo 0
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

function install_prompt {
  # if [ -z "$OLD_PROMPT" ]; then
  #   OLD_PROMPT=$PS1
  # fi

  # if [ -z "$PROMPT_OLD_DIR_WAS_GIT" ]; then
  #   PROMPT_OLD_DIR_WAS_GIT=$(is_git_repo)
  # fi

  if [ -z "$PROMPT_COMMAND" ]; then
    PROMPT_COMMAND="$1"
  else
    PROMPT_COMMAND=${PROMPT_COMMAND%% }; # remove trailing spaces
    PROMPT_COMMAND=${PROMPT_COMMAND%\;}; # remove trailing semi-colon

    case ";$PROMPT_COMMAND;" in
      *";$1;"*)
        # echo "PROMPT_COMMAND already contains: $1"
        :;;
      *)
        PROMPT_COMMAND="$PROMPT_COMMAND;$1"
        # echo "PROMPT_COMMAND does not contain: $1"
        ;;
    esac
  fi
}
add_on_exit install_prompt

__serialise() {
  set -- "${@//\\/\\\\}" # \
    set -- "${@//${FS:-;}/\\${FS:-;}}" # ; - our field separator
    set -- "${@//${RS:-:}/\\${RS:-:}}" # ; - our record separator
    local IFS="${FS:-;}"
    printf ${SERIALIZE_TARGET:+-v"$SERIALIZE_TARGET"} "%s" "$*${RS:-:}"
}
add_on_exit __serialise

__serialise_to() {
  SERIALIZE_TARGET="$1" __serialise "${@:2}"
}
add_on_exit __serialise_to

__unserialise() {
  local IFS="${FS:-;}"
  if test -n "$2"
  then read -d "${RS:-:}" -a "$1" <<<"${*:2}"
  else read -d "${RS:-:}" -a "$1"
  fi
}
add_on_exit __unserialise
