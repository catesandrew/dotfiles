#!/bin/bash
cite 'about-alias'
about-alias 'emacs editor'

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
    (command open -n -a /Applications/Emacs.app "$@") &
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
    (exec emacsclient -c "$@" >/dev/null 2>&1) &
}
