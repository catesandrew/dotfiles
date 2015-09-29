cite about-plugin
about-plugin 'thefuck is a nifty tool that allows you to fix your previous CLI typos'

# [thefuck](https://github.com/nvbn/thefuck) is a nifty tool that allows you to
# fix your previous CLI typos by just typing `fuck`. It perhaps has the
# greatest UX of all products, ever.

[[ `which thefuck` ]] && alias fuck='$(thefuck $(fc -ln -1))'
