if brew_contains_element "gnupg2" || \
    hash gpg 2>/dev/null; then

  export GPG_AGENT_INFO="${HOME}/.gnupg/S.gpg-agent:$(pgrep gpg-agent):1"

  if [ ! -f "${HOME}/.gpg-agent.conf" ]; then
    cat <<EOM >"${HOME}/.gpg-agent.conf"
default-cache-ttl 604800
max-cache-ttl 604800
default-cache-ttl-ssh 604800
max-cache-ttl-ssh 604800
EOM

    if brew_contains_element "pinentry-mac"; then
        cat <<EOM >>"${HOME}/.gpg-agent.conf"
pinentry-program ${BREW_HOME}/bin/pinentry-mac
EOM
    fi
  fi

  if [ -n "${GPG_AGENT_INFO}" ]; then
    socat - UNIX-CONNECT:"${GPG_AGENT_INFO%%:*}" >/dev/null </dev/null

    # if [ "$__dot_system_type" == "Darwin" ]; then
    #   /usr/bin/nc -U "${GPG_AGENT_INFO%%:*}" >/dev/null </dev/null
    # else if brew_contains_element "netcat"; then
    #   nc -U "${GPG_AGENT_INFO%%:*}" >/dev/null </dev/null
    # else
    #   nc -U "${GPG_AGENT_INFO%%:*}" >/dev/null </dev/null
    # fi

    if [ ! -S "${GPG_AGENT_INFO%%:*}" -o $? != 0 ]; then
      # set passphrase cache so I only have to type my passphrase once a day
      eval $(gpg-agent --options "${HOME}/.gpg-agent.conf" --daemon --log-file "${TMPDIR}/gpg-agent.log" --verbose)
    fi
  fi

  # Avoid issues with `gpg` as installed via Homebrew.
  # https://stackoverflow.com/a/42265848/96656
  export GPG_TTY=$(tty)
fi
