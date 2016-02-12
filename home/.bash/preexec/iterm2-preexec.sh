#!/usr/bin/env bash
# This is based on "preexec.bash" but is customized for iTerm2.

# Note: this module requires 2 bash features which you must not otherwise be
# using: the "DEBUG" trap, and the "PROMPT_COMMAND" variable.  iterm2_preexec_install
# will override these and if you override one or the other this _will_ break.

# This is known to support bash3, as well as *mostly* support bash2.05b.  It
# has been tested with the default shells on MacOS X 10.4 "Tiger", Ubuntu 5.10
# "Breezy Badger", Ubuntu 6.06 "Dapper Drake", and Ubuntu 6.10 "Edgy Eft".


# Copy screen-run variables from the remote host, if they're available.

# Saved copy of your PS1. This is used to detect if the user changes PS1
# directly. prev_ps1 will hold the last value that this script set PS1 to
# (including various custom escape sequences). orig_ps1 always holds the last
# user-set value of PS1.
orig_ps1="$PS1"
prev_ps1="$PS1"

# tmux and screen are not supported; even using the tmux hack to get escape
# codes passed through, ncurses interferes and the cursor isn't in the right
# place at the time it's passed through.
if ( [ x"$TERM" != xscreen ] ); then
  # This function is installed as the PROMPT_COMMAND; it is invoked before each
  # interactive prompt display.  It sets a variable to indicate that the prompt
  # was just displayed, to allow the DEBUG trap, below, to know that the next
  # command is likely interactive.
  function iterm2_preexec_invoke_cmd () {
      local s=$?
      last_hist_ent="$(history 1)";
      precmd;
      # This is an iTerm2 addition to try to work around a problem in the
      # original preexec.bash.
      # When the PS1 has command substitutions, this gets invoked for each
      # substitution and each command that's run within the substitution, which
      # really adds up. It would be great if we could do something like this at
      # the end of this script:
      #   PS1="$(iterm2_prompt_prefix)$PS1($iterm2_prompt_suffix)"
      # and have iterm2_prompt_prefix set a global variable that tells precmd not to
      # output anything and have iterm2_prompt_suffix reset that variable.
      # Unfortunately, command substitutions run in subshells and can't
      # communicate to the outside world.
      # Instead, we have this workaround. We save the original value of PS1 in
      # $orig_ps1. Then each time this function is run (it's called from
      # PROMPT_COMMAND just before the prompt is shown) it will change PS1 to a
      # string without any command substitutions by doing eval on orig_ps1. At
      # this point preexec_interactive_mode is still the empty string, so preexec
      # won't produce output for command substitutions.

      if [[ "$PS1" != "$prev_ps1" ]]
      then
        export orig_ps1="$PS1"
      fi

      # Get the value of the prompt prefix, which will change $?
      local iterm2_prompt_prefix_value="$(iterm2_prompt_prefix)"

      # Reset $? to its saved value, which might be used in $orig_ps1.
      sh -c "exit $s"

      # Set PS1 to various escape sequences, the user's preferred prompt, and more escape sequences.
      export PS1="\[$iterm2_prompt_prefix_value\]$orig_ps1\[$(iterm2_prompt_suffix)\]"

      # Save the value we just set PS1 to so if the user changes PS1 we'll know and we can update orig_ps1.
      export prev_ps1="$PS1"
      sh -c "exit $s"
  }

  # -- begin iTerm2 customization

  function iterm2_begin_osc {
    printf "\033]"
  }

  function iterm2_end_osc {
    printf "\007"
  }

  # Runs after interactively edited command but before execution
  function preexec() {
    iterm2_begin_osc
    printf "133;C;\r"
    iterm2_end_osc
  }

  function iterm2_print_state_data() {
    iterm2_begin_osc
    printf "1337;RemoteHost=%s@%s" "$USER" "$iterm2_hostname"
    iterm2_end_osc

    iterm2_begin_osc
    printf "1337;CurrentDir=%s" "$PWD"
    iterm2_end_osc

    iterm2_print_user_vars
  }

  # Usage: iterm2_set_user_var key value
  function iterm2_set_user_var() {
    iterm2_begin_osc
    printf "1337;SetUserVar=%s=%s" "$1" $(printf "%s" "$2" | base64)
    iterm2_end_osc
  }

  # Users can write their own version of this method. It should call
  # iterm2_set_user_var but not produce any other output.
  function iterm2_print_user_vars() {
    true
  }

  function iterm2_prompt_prefix() {
    iterm2_begin_osc
    printf "133;D;\$?"
    iterm2_end_osc

    iterm2_print_state_data

    iterm2_begin_osc
    printf "133;A"
    iterm2_end_osc
  }

  function iterm2_prompt_suffix() {
    iterm2_begin_osc
    printf "133;B"
    iterm2_end_osc
  }

  function iterm2_print_version_number() {
    iterm2_begin_osc
    printf "1337;ShellIntegrationVersion=1"
    iterm2_end_osc
  }


  # If hostname -f is slow on your system, set iterm2_hostname before sourcing this script.
  if [ -z "$iterm2_hostname" ]; then
    iterm2_hostname=$(hostname -f)
  fi
  # iterm2_preexec_install

  # This is necessary so the first command line will have a hostname and current directory.
  # iterm2_print_state_data
  # iterm2_print_version_number
fi
