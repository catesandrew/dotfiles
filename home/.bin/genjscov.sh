#!/bin/bash

USAGE='[help|a|b]'

USAGE='[help|a|b] [<dir>]'
LONG_USAGE=''

die() {
	echo >&2 "$@"
	exit 1
} 

function partA() {
    local dir_name="$1" 
    rm -Rf "${dir_name}"
    pushd "/usr/local/src/walmart/phoenix"> /dev/null  
    lumbar --config ./config/dev.json --use lumbar-long-expires --use conditional --with {\"env\":\"dev\"} --minimize ./lumbar.json build/dev 
    popd > /dev/null
}

function partB() {
    local dir_name="$1" 
    jscoverage "${dir_name}" output
    rm -Rf "${dir_name}"
    mv output "${dir_name}"
}


dashless=$(basename "$0" | sed -e 's/-/ /')
usage() {
  die "Usage: $dashless $USAGE"
}

if [ -z "$LONG_USAGE" ]
then
  LONG_USAGE="Usage: $dashless $USAGE"
else
  LONG_USAGE="Usage: $dashless $USAGE

$LONG_USAGE"
fi

case "$1" in
  -h|--h|--he|--hel|--help)
  echo "$LONG_USAGE"
  exit
esac

case "$#" in
0)
    usage ;;
*)
    cmd="$1"
    shift
    
    case "$cmd" in
    help)
      nf -h ;;
    a)
      partA "$@"
      ;;
    b)
      partB "$@"
      ;;
    *)
      usage 
      ;;
    esac
esac

