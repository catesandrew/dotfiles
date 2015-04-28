cite 'about-alias'
about-alias 'emacs editor'

case $OSTYPE in
  linux*)
    alias em='emacs'
    alias e='emacsclient -n'
    alias E='SUDO_EDITOR="emacsclient" sudo -e'
    ;;
  darwin*)
    # alias em='open -a emacs'
    alias em=__emacs_free
    alias ec=__emacs_client
    alias es=__emacs_stuck
    ;;
esac

function __emacs_free
{
    if [ -e "$@" ]
    then
        # command open -a Emacs "${@}"
        (command open -a /Applications/Emacs.app "$@") &
    else
        touch "$@"
        # command open -a Emacs "${@}"
        (command open -a /Applications/Emacs.app "$@") &
    fi
}

function __emacs_stuck
{
    # $(brew --prefix emacs-mac)/Emacs.app/Contents/MacOS/Emacs "$@"
    # ${BREW_EMACS_MAC_HOME}/Emacs.app/Contents/MacOS/Emacs "$@" & >/dev/null
    # ${BREW_EMACS_MAC_HOME}/Emacs.app/Contents/MacOS/Emacs "$@"
    $(${BREW_EMACS_MAC_HOME}/Emacs.app/Contents/MacOS/Emacs "$@") &
}

function __emacs_client
{
    osascript -e 'tell application "Emacs" to activate'
    exec /usr/bin/env emacsclient -c -n -a "" $*
}
