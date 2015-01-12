#!/usr/bin/env bash

# colored grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;33'

# colored ls
export CLICOLOR=1
export LSCOLORS='Gxfxcxdxdxegedabagacad'

# Load the theme
if [[ $BASH_IT_THEME ]]; then
    source "$BASH_IT/themes/$BASH_IT_THEME/$BASH_IT_THEME.theme.bash"
fi

if [[ $PROMPT_COMMANI ]]; then
    # Make new shells get the history lines from all previous
    # shells instead of the default "last window closed" history
    PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
fi
