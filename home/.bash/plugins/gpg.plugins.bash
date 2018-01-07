if brew_contains_element "gnupg2" || \
    hash gpg 2>/dev/null; then

  # Avoid issues with `gpg` as installed via Homebrew.
  # https://stackoverflow.com/a/42265848/96656
  export GPG_TTY=$(tty)
fi
