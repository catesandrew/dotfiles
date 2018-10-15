if brew_contains_element "keychain" || \
    hash keychain 2>/dev/null; then

  eval $(keychain --quiet --noask --eval --agents gpg,ssh --inherit any --timeout 28800 id_rsa)
fi
