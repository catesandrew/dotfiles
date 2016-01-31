# make sure virtualenvwrapper is enabled if available

cite about-plugin
about-plugin 'virtualenvwrapper helper functions'

if ! hash virtualenvwrapper.sh 2>/dev/null; then
    exit 0;
fi

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
export VIRTUALENVWRAPPER_SCRIPT=/usr/local/bin/virtualenvwrapper.sh
source /usr/local/bin/virtualenvwrapper_lazy.sh

function mkvenv {
  about 'create a new virtualenv for this directory'
  group 'virtualenv'

  cwd=`basename \`pwd\``
  mkvirtualenv --distribute $cwd
}


function mkvbranch {
  about 'create a new virtualenv for the current branch'
  group 'virtualenv'

  mkvirtualenv --distribute "$(basename `pwd`)@$SCM_BRANCH"
}

function wovbranch {
  about 'sets workon branch'
  group 'virtualenv'

  workon "$(basename `pwd`)@$SCM_BRANCH"
}

function wovenv {
  about 'works on the virtualenv for this directory'
  group 'virtualenv'

  workon "$(basename `pwd`)"
}
