# Some aliases for thefuck

cite 'about-alias'
about-alias 'the fuck abbreviations'

if ! brew_contains_element "thefuck"; then
    exit 0;
fi

alias fuck='PYTHONIOENCODING=utf-8 eval $(thefuck $(fc -ln -1)); history -r'
# old
# alias fuck='$(thefuck $(fc -ln -1))'
