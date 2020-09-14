if brew_contains_element "gnupg2" || \
    hash gpg 2>/dev/null; then

  # https://www.gnupg.org/faq/whats-new-in-2.1.html#autostart
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

  AGENT_PORT=$(pgrep gpg-agent)

  if [ -n "${AGENT_PORT}" ]; then
    AGENT_SOCKET=$(gpgconf --list-dirs agent-socket)
    GPG_AGENT_INFO="${AGENT_SOCKET}:${AGENT_PORT}:1"

    if [ "$(socat - UNIX-CONNECT:"${GPG_AGENT_INFO%%:*}" </dev/null)" ]; then
      if [ ! -S "${GPG_AGENT_INFO%%:*}" -o $? != 0 ]; then
        # set passphrase cache so I only have to type my passphrase once a day
        eval $(gpg-agent --options ${HOME}/.gpg-agent.conf --daemon --log-file "${TMPDIR}/gpg-agent.log" --verbose)
      fi
    fi

    export GPG_AGENT_INFO
  else
    echo "starting gpg-agent daemon"

    # set passphrase cache so I only have to type my passphrase once a day
    eval $(gpg-agent --options ${HOME}/.gpg-agent.conf --daemon --log-file "${TMPDIR}/gpg-agent.log" --verbose)

    # AGENT_PORT=$(pgrep gpg-agent)
    # AGENT_SOCKET=$(gpgconf --list-dirs agent-socket)
    # export GPG_AGENT_INFO
  fi

  # Avoid issues with `gpg` as installed via Homebrew.
  # https://stackoverflow.com/a/42265848/96656
  export GPG_TTY=$(tty)
fi
