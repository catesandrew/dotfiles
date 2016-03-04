# If not running interactively, don't do anything
[[ $- != *i*  ]] && return

if [ -f "${HOME}/.bashrc" ]; then
  . "${HOME}/.bashrc"
fi
