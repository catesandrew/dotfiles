if brew_contains_element "yarn" || \
    hash yarn 2>/dev/null; then

  yarn_remove_all () {
    yarn global remove $(yarn global list | \grep info | sed 's/^info "\(.*\)@.*".*$/\1/')
  }
fi
