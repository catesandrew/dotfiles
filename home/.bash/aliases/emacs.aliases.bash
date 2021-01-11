#!/bin/bash

case $OSTYPE in
  linux*)
    alias em='emacs'
    alias ec='emacsclient -c -a ""'
    ;;
  darwin*)
    alias em=__emacs_free
    alias ec=__emacs_client
    ;;
esac

function __emacs_free
{
  # start new instances with -n -a
  # (command open -n -a /Applications/Emacs.app "$@") &
  (${BREW_HOME}/bin/emacs "$@" >/dev/null 2>&1) &
}

# Number of current visible frames,
# Emacs daemon always has a visible frame called F1
em_visible_frames() {
  "${BREW_HOME}/bin/emacsclient" -e '(length (visible-frame-list))'
}

em_change_focus() {
  "${BREW_HOME}/bin/emacsclient" -n -e "(select-frame-set-input-focus (selected-frame))" > /dev/null
}

function __emacs_client
{
  # version 1
  # (exec emacsclient -c -a "" "$@") &

  # version 2
  # http://stackoverflow.com/questions/778716/how-can-i-make-emacs-start-up-faster
  # (exec emacsclient -c -e "(let ((default-directory \"$(pwd)/\")) (select-frame (make-frame)) (find-file \"$1\"))" 2&>/dev/null) &

  # version 3
  # emacsclient -t $@ || (emacs --daemon && emacsclient -t $@)
  # (exec ${BREW_HOME}/bin/emacsclient -c "$@" >/dev/null 2>&1) &

  # version 4 shamelessly taken from http://mjwall.com/blog/2013/10/04/how-i-use-emacs/

  # emacsclient options for reference
  # -a "" starts emacs daemon and reattaches
  # -c creates a new frame
  # -n returns control back to the terminal
  # -e eval the script

  # try switching to the frame incase it is just minimized
  test "$(em_visible_frames)" -eq "1" && em_change_focus

  if [ "$(em_visible_frames)" -lt  "2" ]; then # need to create a frame
    # -c $@ with no args just opens the scratch buffer
    "${BREW_HOME}/bin/emacsclient" -n -c "$@" && em_change_focus
    "${BREW_HOME}/bin/emacsclient" -n -e "(set-frame-size (selected-frame) 150 48)" > /dev/null
  else # there is already a visible frame besides the daemon, so
    em_change_focus
    # -n $@ errors if there are no args
    test  "$#" -ne "0" && "${BREW_HOME}/bin/emacsclient" -n "$@"
  fi
}
