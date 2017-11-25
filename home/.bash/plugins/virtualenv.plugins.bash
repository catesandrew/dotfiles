# make sure virtualenvwrapper is enabled if available
# 'virtualenvwrapper helper functions'

if hash virtualenvwrapper.sh 2>/dev/null; then

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
export VIRTUALENVWRAPPER_SCRIPT=/usr/local/bin/virtualenvwrapper.sh
source /usr/local/bin/virtualenvwrapper_lazy.sh

# create a new virtualenv for this directory
function mkvenv {
  cwd=`basename \`pwd\``
  mkvirtualenv --distribute $cwd
}

# create a new virtualenv for the current branch
function mkvbranch {
  mkvirtualenv --distribute "$(basename `pwd`)@$SCM_BRANCH"
}

# sets workon branch
function wovbranch {
  workon "$(basename `pwd`)@$SCM_BRANCH"
}

# works on the virtualenv for this directory
function wovenv {
  workon "$(basename `pwd`)"
}

fi
