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
