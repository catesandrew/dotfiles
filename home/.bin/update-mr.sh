#!/bin/bash
# Update all git directories below current directory or specified directory
# Skips directories that contain a file called .ignore
# Usage update-mr.sh <destination> <mr-directory>

function update {
  local d="$1"
  if [ -d "$d" ]; then
    cd $d > /dev/null
    if [ -d ".git" ]; then
      echo -e "\nUpdating `pwd`"
      mr -d "$2" -j 5 register
      # git pull
    else
      scan *
    fi
    cd .. > /dev/null
  fi
  #echo "Exiting update: pwd=`pwd`"
}

function scan {
  #echo "`pwd`"
  for x in $*; do
    update "$x"
  done
}

if [ "$1" != "" ]; then cd $1 > /dev/null; fi
echo -e "Scanning ${PWD}"
scan *
