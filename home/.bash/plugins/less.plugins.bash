if brew_contains_element "lesspipe"; then
  export LESSOPEN="|${BREW_HOME}/bin/lesspipe.sh %s" LESS_ADVANCED_PREPROCESSOR=1
elif hash lesspipe 2>/dev/null; then
  # make less more friendly for non-text input files, see lesspipe(1)
  [ -x "$(which lesspipe)" ] && eval "$(SHELL=/bin/sh lesspipe)"
fi
