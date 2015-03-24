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
        command open -a Emacs "${@}"
    else
        touch "$@"
        command open -a Emacs "${@}"
    fi
}

function __emacs_stuck
{
    ${BREW_HOME}/homebrew/Cellar/emacs-mac/HEAD/Emacs.app/Contents/MacOS/Emacs "$@"

}

function __emacs_client
{
    osascript -e 'tell application "Emacs" to activate'
    # /usr/local/bin/emacsclient -c -n -a "" "$@"
    exec /usr/bin/env emacsclient -c -n -a "" $*
}

