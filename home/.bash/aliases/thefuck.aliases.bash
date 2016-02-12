# Some aliases for thefuck

if brew_contains_element "thefuck"; then
  # alias fuck='$(thefuck $(fc -ln -1))'
  alias fuck='PYTHONIOENCODING=utf-8 eval $(thefuck $(fc -ln -1)); history -r'
fi
