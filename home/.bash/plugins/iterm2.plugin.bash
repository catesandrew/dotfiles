cite about-plugin
about-plugin 'load iterm2, if you are using it'

if [ "${INSIDE_EMACS:-""}" != "" ]; then
    :
elif [ "${TERM_PROGRAM:-""}" != "" ]; then
    if ( [ "$TERM_PROGRAM" == "iTerm.app" ] ); then
    echo "TERM_PROGRAM = $TERM_PROGRAM";
        [ -f ~/.iterm2_shell_integration.bash ] && source ~/.iterm2_shell_integration.bash
    fi
fi
