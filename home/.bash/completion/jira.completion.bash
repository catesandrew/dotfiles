if brew_contains_element "go-jira" || \
    hash jira 2>/dev/null; then

  # https://gohugo.io/templates/introduction/
  # https://golang.org/pkg/text/template/
  eval "$(jira --completion-script-bash)"
fi
