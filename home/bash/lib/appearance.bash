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

# If we want to have bash immediately add commands to our history instead of
# waiting for the end of each session (to enable commands in one terminal to be
# instantly be available in another), we can also set or append the history -a
# command to the PROMPT_COMMAND parameter, which contains commands that are
# executed before each new command prompt.

# To do this correctly, we need to do a bit of a hack. We need to append to the
# history file immediately with history -a, clear the current history in our
# session with history -c, and then read the history file that we've appended
# to, back into our session history with history -r.

if [[ $PROMPT_COMMAND ]]; then
    # Make new shells get the history lines from all previous
    # shells instead of the default "last window closed" history
    PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
fi
