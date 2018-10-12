if brew_contains_element "gnupg2" || \
    hash gpg 2>/dev/null; then

  if [ -f "${HOME}/.gpg-agent-info" ]; then
    . "${HOME}/.gpg-agent-info"
    export GPG_AGENT_INFO
  fi

  if [ ! -f "${HOME}/.gpg-agent.conf" ]; then
    cat <<EOM >"${HOME}/.gpg-agent.conf"
default-cache-ttl 604800
max-cache-ttl 604800
default-cache-ttl-ssh 604800
max-cache-ttl-ssh 604800
EOM

    if brew_contains_element "pinentry-mac"; then
        cat <<EOM >>"${HOME}/.gpg-agent.conf"
use-standard-socket
pinentry-program ${BREW_HOME}/bin/pinentry-mac
EOM
    fi
  fi


  if [ -n "${GPG_AGENT_INFO}" ]; then
    nc  -U "${GPG_AGENT_INFO%%:*}" >/dev/null </dev/null
    if [ ! -S "${GPG_AGENT_INFO%%:*}" -o $? != 0 ]; then
      # set passphrase cache so I only have to type my passphrase once a day
      eval $(gpg-agent --options "${HOME}/.gpg-agent.conf" --daemon --write-env-file "${HOME}/.gpg-agent-info" --use-standard-socket --log-file "${HOME}/tmp/gpg-agent.log" --verbose)
    fi
  fi

  # Avoid issues with `gpg` as installed via Homebrew.
  # https://stackoverflow.com/a/42265848/96656
  export GPG_TTY=$(tty)
fi
