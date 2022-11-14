# Add the following to ~/.gnupg/gpg.conf
# auto-key-retrieve
# no-emit-version
# keyid-format 0xlong
# keyserver hkps://keys.openpgp.org

# upload key:
# gpg --export your_address@example.net | curl -T - https://keys.openpgp.org

if brew_contains_element "gnupg2" || \
    hash gpg 2>/dev/null; then

  if [ ! -d "${HOME}/.gnupg" ]; then
    mkdir -p "${HOME}/.gnupg"
  fi

  # https://www.gnupg.org/faq/whats-new-in-2.1.html#autostart
  if [ ! -f "${HOME}/.gnupg/gpg-agent.conf" ]; then
    cat <<EOM >"${HOME}/.gnupg/gpg-agent.conf"
default-cache-ttl 6048000
max-cache-ttl 6048000
default-cache-ttl-ssh 6048000
max-cache-ttl-ssh 6048000
enable-ssh-support
EOM

    if brew_contains_element "pinentry-mac"; then
        cat <<EOM >>"${HOME}/.gnupg/gpg-agent.conf"
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
        eval $(gpg-agent --options "${HOME}/.gnupg/gpg-agent.conf" --daemon --log-file "${TMPDIR}/gpg-agent.log" --verbose)
      fi
    fi
  else
    echo "starting gpg-agent daemon"

    # set passphrase cache so I only have to type my passphrase once a day
    eval $(gpg-agent --options "${HOME}/.gnupg/gpg-agent.conf" --daemon --log-file "${TMPDIR}/gpg-agent.log" --verbose)

    # AGENT_PORT=$(pgrep gpg-agent)
    # AGENT_SOCKET=$(gpgconf --list-dirs agent-socket)
  fi

  # Avoid issues with `gpg` as installed via Homebrew.
  # https://stackoverflow.com/a/42265848/96656
  GPG_TTY=$(tty)
  export GPG_TTY
fi
