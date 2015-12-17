cite about-plugin
about-plugin 'load iterm2, if you are using it'

export ITERM_24BIT=1
export KONSOLE_DBUS_SESSION=1

function title {
  # no op
  echo -n
}

if [ "${INSIDE_EMACS:-""}" != "" ]; then
    :
elif [ "${TERM_PROGRAM:-""}" != "" ]; then
    if ( [ "$TERM_PROGRAM" == "iTerm.app" ] ); then
        [ -f ~/.iterm2_shell_integration.bash ] && source ~/.iterm2_shell_integration.bash
        function title {
            echo -ne "\033]0;"$*"\007"
        }
    fi
elif [ -n "$SESSION_TYPE" ]; then
    [ -f ~/.iterm2_shell_integration.bash ] && source ~/.iterm2_shell_integration.bash
fi

