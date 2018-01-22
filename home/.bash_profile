# If not running interactively, don't do anything
[[ $- != *i*  ]] && return

if [ -f "${HOME}/.bashrc.load" ]; then
  . "${HOME}/.bashrc.load"
fi

