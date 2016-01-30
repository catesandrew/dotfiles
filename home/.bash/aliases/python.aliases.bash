# Some aliases for python

cite 'about-alias'
about-alias 'python abbreviations'

# Update Python utilities.
alias pup='pip install --upgrade setuptools && pip install --upgrade pip'

# http://mikegrouchy.com/blog/2014/06/pro-tip-pip-upgrade-all-python-packages.html
alias pipup="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip install -U"
