#!/bin/bash
# This is based on "preexec.bash" but is customized for iTerm2.
# https://iterm2.com/misc/bash_startup.in

# Note: this module requires 2 bash features which you must not otherwise be
# using: the "DEBUG" trap, and the "PROMPT_COMMAND" variable.  iterm2_preexec_install
# will override these and if you override one or the other this _will_ break.

# This is known to support bash3, as well as *mostly* support bash2.05b.  It
# has been tested with the default shells on MacOS X 10.4 "Tiger", Ubuntu 5.10
# "Breezy Badger", Ubuntu 6.06 "Dapper Drake", and Ubuntu 6.10 "Edgy Eft".

# tmux and screen are not supported; even using the tmux hack to get escape
# codes passed through, ncurses interferes and the cursor isn't in the right
# place at the time it's passed through.
if [[ "$TERM" != screen && "$ITERM_SHELL_INTEGRATION_INSTALLED" = "" && "$-" == *i* ]]; then
  ITERM_SHELL_INTEGRATION_INSTALLED=Yes
  # Saved copy of your PS1. This is used to detect if the user changes PS1
  # directly. ITERM_PREV_PS1 will hold the last value that this script set PS1 to
  # (including various custom escape sequences).
  ITERM_PREV_PS1="$PS1"

  # This function is installed as the PROMPT_COMMAND; it is invoked before each
  # interactive prompt display.  It sets a variable to indicate that the prompt
  # was just displayed, to allow the DEBUG trap, below, to know that the next
  # command is likely interactive.
  function __iterm2_precmd () {
   __iterm2_last_ret_value="$?"

    # Work around a bug in CentOS 7.2 where preexec doesn't run if you press
    # ^C while entering a command.
    if [[ -z "${iterm2_ran_preexec:-}" ]]
    then
        __iterm2_preexec ""
    fi
    iterm2_ran_preexec=""

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
    # $ITERM_ORIG_PS1. Then each time this function is run (it's called from
    # PROMPT_COMMAND just before the prompt is shown) it will change PS1 to a
    # string without any command substitutions by doing eval on ITERM_ORIG_PS1. At
    # this point ITERM_PREEXEC_INTERACTIVE_MODE is still the empty string, so preexec
    # won't produce output for command substitutions.

    # The first time this is called ITERM_ORIG_PS1 is unset. This tests if the variable
    # is undefined (not just empty) and initializes it. We can't initialize this at the
    # top of the script because it breaks with liquidprompt. liquidprompt wants to
    # set PS1 from a PROMPT_COMMAND that runs just before us. Setting ITERM_ORIG_PS1
    # at the top of the script will overwrite liquidprompt's PS1, whose value would
    # never make it into ITERM_ORIG_PS1. Issue 4532. It's important to check
    # if it's undefined before checking if it's empty because some users have
    # bash set to error out on referencing an undefined variable.
    if [ -z "${ITERM_ORIG_PS1+xxx}" ]
    then
      # ITERM_ORIG_PS1 always holds the last user-set value of PS1.
      # You only get here on the first time iterm2_preexec_invoke_cmd is called.
      export ITERM_ORIG_PS1="$PS1"
    fi

    if [[ "$PS1" != "$ITERM_PREV_PS1" ]]
    then
      export ITERM_ORIG_PS1="$PS1"
    fi

    # Get the value of the prompt prefix, which will change $?
    \local iterm2_prompt_prefix_value="$(iterm2_prompt_prefix)"

    # Add the mark unless the prompt includes '$(iterm2_prompt_mark)' as a substring.
    if [[ $ITERM_ORIG_PS1 != *'$(iterm2_prompt_mark)'* ]]
    then
      iterm2_prompt_prefix_value="$iterm2_prompt_prefix_value$(iterm2_prompt_mark)"
    fi

    # Send escape sequences with current directory and hostname.
    iterm2_print_state_data

    # Reset $? to its saved value, which might be used in $ITERM_ORIG_PS1.
    __bp_set_ret_value "$__iterm2_last_ret_value" "$__bp_last_argument_prev_command"

    # Set PS1 to various escape sequences, the user's preferred prompt, and more escape sequences.
    export PS1="\[$iterm2_prompt_prefix_value\]$ITERM_ORIG_PS1\[$(iterm2_prompt_suffix)\]"

    # Save the value we just set PS1 to so if the user changes PS1 we'll know and we can update ITERM_ORIG_PS1.
    export ITERM_PREV_PS1="$PS1"
    __bp_set_ret_value "$__iterm2_last_ret_value" "$__bp_last_argument_prev_command"

  }

  # This function is installed as the DEBUG trap.  It is invoked before each
  # interactive prompt display.  Its purpose is to inspect the current
  # environment to attempt to detect if the current command is being invoked
  # interactively, and invoke 'preexec' if so.
  function __iterm2_preexec () {
    # Save the returned value from our last command
    __iterm2_last_ret_value="$?"

    iterm2_begin_osc
    printf "133;C;"
    iterm2_end_osc
    # If PS1 still has the value we set it to in iterm2_preexec_invoke_cmd then
    # restore it to its original value. It might have changed if you have
    # another PROMPT_COMMAND (like liquidprompt) that modifies PS1.
    if [ -n "${ITERM_ORIG_PS1+xxx}" -a "$PS1" = "$ITERM_PREV_PS1" ]
    then
      export PS1="$ITERM_ORIG_PS1"
    fi
    iterm2_ran_preexec="yes"

    __bp_set_ret_value "$__iterm2_last_ret_value" "$__bp_last_argument_prev_command"
  }

  # We don't care about whitespace, but users care about not changing their histcontrol variables.
  # We overwrite the upstream __bp_adjust_histcontrol function whcih gets called from the next
  # PROMPT_COMMAND invocation.
  function __bp_adjust_histcontrol() {
    true
  }

  function iterm2_begin_osc {
    printf "\033]"
  }

  function iterm2_end_osc {
    printf "\007"
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
    printf "1337;SetUserVar=%s=%s" "$1" $(printf "%s" "$2" | base64 | tr -d '\n')
    iterm2_end_osc
  }

  if [ -z "$(type -t iterm2_print_user_vars)" ] || [ "$(type -t iterm2_print_user_vars)" != function ]; then
    # iterm2_print_user_vars is not already defined. Provide a no-op default version.
    #
    # Users can write their own version of this function. It should call
    # iterm2_set_user_var but not produce any other output.
    function iterm2_print_user_vars() {
      # For every function defined in our function array. Invoke it.
      local iterm2_function
      for iterm2_function in "${iterm2_print_user_vars_functions[@]}"; do

        # Only execute this function if it actually exists.
        # Test existence of functions with: declare -[Ff]
        if type -t "$iterm2_function" 1>/dev/null; then
          $iterm2_function
        fi
      done
    }
  fi

  function iterm2_prompt_prefix() {
    iterm2_begin_osc
    printf "133;D;\$?"
    iterm2_end_osc
  }

  function iterm2_prompt_mark() {
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
    printf "1337;ShellIntegrationVersion=9;shell=bash"
    iterm2_end_osc
  }

  # If hostname -f is slow on your system, set iterm2_hostname before sourcing this script.
  if [ -z "${iterm2_hostname:-}" ]; then
    iterm2_hostname=$(hostname -f 2>/dev/null)
    # some flavors of BSD (i.e. NetBSD and OpenBSD) don't have the -f option
    if [ $? -ne 0 ]; then
      iterm2_hostname=$(hostname)
    fi
  fi
  # iterm2_preexec_install

  # This is necessary so the first command line will have a hostname and current directory.
  # iterm2_print_state_data
  # iterm2_print_version_number
fi
