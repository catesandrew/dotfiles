#!/bin/sh
# This function performs app completion based on known applications
# Originally by Kim Holburn http://www.holburn.net/
# Modified by Brett Terpstra because it wasn't working on 10.6.
# Added case insensitivity and LC_ALL='C' because unicode chars were breaking it.
# Added geticon for geticon() function in .bash_profile

export appslist=~/.apps.list

_make_app_list () {
  local LC_ALL='C'
  mdfind -onlyin /Applications -onlyin /Developer "kMDItemContentType == 'com.apple.application-*'" | \
  while read ; do
     echo "${REPLY##*/}"
  done |sort -i > "$appslist"
}

_apple_open ()
{
  local cur prev
  local LC_ALL='C'
  # renew appslist if it's older than a day
  if ! /usr/bin/perl -e '
    my $ARGV = $ARGV[0];
    if (-e $ARGV) { if (time - (stat $ARGV)[9] <= 86400) { exit (0); } }
    exit 1;
  ' "$appslist" ; then
    _make_app_list
  fi

  COMPREPLY=()
  cur=${COMP_WORDS[COMP_CWORD]}
  prev=${COMP_WORDS[COMP_CWORD-1]}

  # do not attempt completion if we're specifying an option
  [[ "$cur" == -* ]] && return 0

  if [[ "$prev" == '-a' || "$prev" == 'o' || "$prev" == 'geticon' ]]; then

    # If we have an appslist
    if [ -s "$appslist" -a -r "$appslist" ]; then
      # Escape dots in paths for grep
      cur=${cur//\./\\\.}

      local IFS="
"
      COMPREPLY=( $( grep -i "^$cur" "$appslist" | sed -e 's/ /\\ /g' ) )

    fi
  else
    _filedir
  fi

  return 0
}

complete -o bashdefault -o default -o nospace -F _apple_open open 2>/dev/null || complete -o default -o nospace -F _apple_open open
complete -o bashdefault -o default -o nospace -F _apple_open geticon 2>/dev/null || complete -o default -o nospace -F _apple_open geticon
