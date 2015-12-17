#!/usr/bin/env bash

# colored grep
export GREP_COLOR='1;33'

# colored ls
export CLICOLOR=1
export LSCOLORS='Gxfxcxdxdxegedabagacad'

# Load the theme
if [[ $BASH_IT_THEME ]]; then
    source "$BASH_IT/themes/$BASH_IT_THEME/$BASH_IT_THEME.theme.bash"
fi

# Make commands in one terminal instantly available to commands in another.
export PROMPTED=false
export PROMPT_COMMAND="if [[ \$PROMPTED = true ]]; then echo ''; fi; export PROMPTED=true; $PROMPT_COMMAND"
