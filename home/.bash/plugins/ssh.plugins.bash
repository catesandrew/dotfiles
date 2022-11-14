# https://rabexc.org/posts/pitfalls-of-ssh-agents
# https://stackoverflow.com/questions/18880024/start-ssh-agent-on-login
# https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/ssh-agent/ssh-agent.plugin.zsh
if brew_contains_element "openssh" || \
    hash ssh 2>/dev/null; then

  SSH_ENV="$HOME/.ssh/environment"

  # approach 1
  # this keeps the details of the agent in a file, tries to load it, checks if
  # that agent is still running (after a reboot or similar), and if not, it
  # starts another one.
  function start_agent {
    echo "Initialising new SSH agent..."
    (umask 066; ${BREW_HOME}/bin/ssh-agent > "${SSH_ENV}")
    . "${SSH_ENV}" > /dev/null
    ${BREW_HOME}/bin/ssh-add
  }

  # Source SSH settings, if applicable
  if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    ps -ef | \grep ${SSH_AGENT_PID} | \grep ssh-agent$ > /dev/null || {
      start_agent;
    }
  else
    start_agent;
  fi

  # approach 2
  # queries the agent for available keys. If none can be found, it will try to
  # load the agent config from a file, and if still can't connect to the agent,
  # it will start a new one.
  # ${BREW_HOME}/bin/ssh-add -l &>/dev/null
  # if [ "$?" == 2 ]; then
  #   test -r "${SSH_ENV}" && \
    #     eval "$(<"${SSH_ENV}")" >/dev/null

  #   ${BREW_HOME}/bin/ssh-add -l &>/dev/null
  #   if [ "$?" == 2 ]; then
  #     (umask 066; ${BREW_HOME}/bin/ssh-agent > "${SSH_ENV}")
  #     eval "$(<"${SSH_ENV}")" >/dev/null
  #     ${BREW_HOME}/bin/ssh-add
  #   fi
  # fi

fi
