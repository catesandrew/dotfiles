cite about-plugin
about-plugin 'load pyenv, if you are using it'

# Node Version Manager
if hash brew 2>/dev/null; then
    if [ x"" != x"$(brew ls --versions pyenv)" ]; then
        export PYENV_ROOT=/usr/local/var/pyenv
    fi
else
    export PYENV_ROOT="$HOME/.pyenv"
fi
pathmunge $PYENV_ROOT

[[ `which pyenv` ]] && eval "$(pyenv init -)"

#Load pyenv virtualenv if the virtualenv plugin is installed.
if pyenv virtualenv-init - &> /dev/null; then
  eval "$(pyenv virtualenv-init -)"
fi

# Load the auto-completion script if pyenv was loaded.
[[ -e $PYENV_ROOT/completions/pyenv.bash ]] && source $PYENV_ROOT/completions/pyenv.bash
