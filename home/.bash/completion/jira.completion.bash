if brew_contains_element "go-jira" || \
    hash jira 2>/dev/null; then

  eval "$(jira --completion-script-bash)"
fi
