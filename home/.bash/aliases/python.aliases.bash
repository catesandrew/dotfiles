# Some aliases for python

# Update Python utilities.
alias pup='pip install --upgrade setuptools && pip install --upgrade pip'
alias pup2='pip2 install --upgrade setuptools && pip2 install --upgrade pip'
alias pup3='pip3 install --upgrade setuptools && pip3 install --upgrade pip'

# http://mikegrouchy.com/blog/2014/06/pro-tip-pip-upgrade-all-python-packages.html
alias pipup="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip install -U"
alias pipup2="pip2 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip2 install -U"
alias pipup3="pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip3 install -U"
