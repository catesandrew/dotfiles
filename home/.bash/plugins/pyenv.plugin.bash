cite about-plugin
about-plugin 'load pyenv, if you are using it'

if brew_contains_element "pyenv"; then
    # export PYENV_HOME="$(brew --prefix pyenv)"
    export PYENV_HOME="$__dot_brew_home/opt/pyenv"
    export PYENV_ROOT="$__dot_brew_home/var/pyenv"
else
    export PYENV_ROOT="$HOME/.pyenv"
fi

if [ -n $PYENV_HOME ]; then
    # Load the auto-completion script if pyenv was loaded.
    # [[ -e $PYENV_HOME/completions/pyenv.bash ]] && \
    #     source $PYENV_HOME/completions/pyenv.bash

    # [[ `which pyenv` ]] && eval "$(pyenv init -)"

    #Load pyenv virtualenv if the virtualenv plugin is installed.
    # if pyenv virtualenv-init - &> /dev/null; then
    #   eval "$(pyenv virtualenv-init -)"
    # fi

    # lazy load pyenv into a shell session *as a function*
    pyenv() {
        [ -s "$PYENV_HOME/completions/pyenv.bash" ] && \
            . $PYENV_HOME/completions/pyenv.bash

        eval "$(pyenv init -)"

        #Load pyenv virtualenv if the virtualenv plugin is installed.
        if pyenv virtualenv-init - &> /dev/null; then
            eval "$(pyenv virtualenv-init -)"
        fi

        pyenv "$@"
    }
fi
