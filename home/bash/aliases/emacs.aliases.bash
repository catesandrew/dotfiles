cite 'about-alias'
about-alias 'emacs editor'

case $OSTYPE in
  linux*)
    alias em='emacs'
    alias e='emacsclient -n'
    alias E='SUDO_EDITOR="emacsclient" sudo -e'
    ;;
  darwin*)
    alias em=__emacs_free
    alias ec=__emacs_client
    alias memacs=__emacs_stuck
    ;;
esac

function __emacs_free
{
    if [ -e "$@" ]
    then
        command open -a Emacs "${@}"
    else
        touch "$@"
        command open -a Emacs "${@}"
    fi
}

function __emacs_stuck
{
    # $(brew --prefix emacs-mac)/Emacs.app/Contents/MacOS/Emacs "$@"
    ${BREW_EMACS_MAC_HOME}/Emacs.app/Contents/MacOS/Emacs "$@" & >/dev/null
}

function __emacs_client
{
    osascript -e 'tell application "Emacs" to activate'
    exec /usr/bin/env emacsclient -c -n -a "" $*
}

