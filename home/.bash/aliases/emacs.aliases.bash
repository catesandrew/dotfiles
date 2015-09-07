cite 'about-alias'
about-alias 'emacs editor'

case $OSTYPE in
  linux*)
    alias em='emacs'
    alias ec='emacsclient -n'
    ;;
  darwin*)
    # alias em='open -a emacs'
    alias em=__emacs_free
    alias ec=__emacs_client
    ;;
esac

function __emacs_free
{
    if [ -e "$@" ]
    then
        # start new instances with -n -a
        (command open -n -a /Applications/Emacs.app "$@") &
    else
        touch "$@"
        # start new instances with -n -a
        (command open -n -a /Applications/Emacs.app "$@") &
    fi
}

function __emacs_client
{
    osascript -e 'tell application "Emacs" to activate'
    exec /usr/bin/env emacsclient -c -n -a "" $*
}
