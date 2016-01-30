# Load rbebv, if you are using it
cite about-plugin
about-plugin 'load rbenv, if you are using it'

if brew_contains_element "rbenv"; then
    # export RBENV_ROOT="$__dot_brew_home/var/rbenv"
    export RBENV_ROOT="$HOME/.rbenv"
else
    export RBENV_ROOT="$HOME/.rbenv"
fi

[[ `which rbenv` ]] && eval "$(rbenv init -)"

pathmunge "${RBENV_ROOT}/shims"

if [ -n $RBENV_HOME ]; then
    # Load the auto-completion script if rbenv was loaded.
    [[ -e $RBENV_HOME/completions/rbenv.bash ]] && \
        source $RBENV_HOME/completions/rbenv.bash
fi
