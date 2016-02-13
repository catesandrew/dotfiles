#!/usr/bin/env bash

# Load the theme
if [[ $BASH_IT_THEME ]]; then
    . "$BASH_IT/themes/$BASH_IT_THEME/$BASH_IT_THEME.theme.bash"
fi

if [ -z "$INSIDE_EMACS" ]; then
    # Make commands in one terminal instantly available to commands in another.
    export PROMPTED=false
    export PROMPT_COMMAND="if [[ \$PROMPTED = true ]]; then echo ''; fi; export PROMPTED=true; $PROMPT_COMMAND"
fi
