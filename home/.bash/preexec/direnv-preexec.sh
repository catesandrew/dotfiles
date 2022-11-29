#
# direnv-preexec.sh
#

# Avoid duplicate inclusion
if [[ "$__direnv_imported" == "defined" ]]; then
    return 0
fi
__direnv_imported="defined"

__direnv_setup() {
    # check that we're using bash
    if [[ -n $BASH_VERSION ]]; then
        __direnv_hook
    fi
}

__direnv_hook() {
    # Hook into preexec and precmd functions
    if ! contains_element __direnv_bash_preexec "${preexec_functions[@]}"; then
        preexec_functions+=(__direnv_bash_preexec)
    fi

    if ! contains_element __direnv_bash_precmd "${precmd_functions[@]}"; then
        precmd_functions+=(__direnv_bash_precmd)
    fi
}

#
# Function to be run by our preexec hook.
#
# Saves the directory this command is being executed in to track (cd-ing), and
# sets a variable so we know that a command was just executed and should be
# saved.
#
# GLOBALS:
#   __DIRENV_PWD The directory this command is being executed in
#   __DIRENV_SAVE_COMMAND The command that is being executed and to be saved.
#
# Arguments:
#  $1 The command just entered, about to be executed.
#
__direnv_bash_preexec() {
  __DIRENV_PWD="$PWD"
  __DIRENV_SAVE_COMMAND="$1"
}

# _direnv_hook() {
#   local previous_exit_status=$?;
#   trap -- '' SIGINT;
#   eval "$("/usr/local/bin/direnv" export bash)";
#   trap - SIGINT;
#   return $previous_exit_status;
# };

__direnv_bash_precmd() {
  # Set this initially to properly catch the exit status.
  __DIRENV_EXIT_STATUS="$?"

  local command="$__DIRENV_SAVE_COMMAND"

  # Check if we need to process a command. If so, unset it as it will be
  # processed and saved.
  # TODO only run with cd, popd, pushd commands?
  # if [[ -n "$__DIRENV_SAVE_COMMAND" ]]; then
  #   unset __DIRENV_SAVE_COMMAND
  # else
  #   return 0
  # fi

  __direnv_process_command "$command"
}

#
# Send our command to the server if everything
# looks good.
#
# @param A trimmed command from the command line
#
__direnv_process_command() {
  # TODO only run with cd, popd, pushd commands
  # local direnv_command
  # direnv_command=$(__direnv_trim_whitespace "$1")

  # Sanity empty check
  # if [[ -z "$direnv_command" ]]; then
  #   return 0;
  # fi;

  # Check to make sure direnv is still installed. Otherwise, this will
  # simply fail and spam the user that files dont exist.
  # if ! hash direnv &> /dev/null; then
  #   return 0;
  # fi;

  # local process_id=$$

  # This is non-standard across systems. GNU Date and BSD Date
  # both convert to epoch differently. Using python for cross system
  # compatibility.
  # local process_start_stamp
  # process_start_stamp=$(LC_ALL=C ps -p $$ -o lstart=)
  # local process_start=$(bashhub util parsedate "$process_start_stamp")

  # local working_directory="$__BH_PWD"
  local exit_status="$__BH_EXIT_STATUS"

  eval "$("direnv" export bash)"
  return $exit_status
}

__direnv_trim_whitespace() {
  local var=$@
  var="${var#"${var%%[![:space:]]*}"}"   # remove leading whitespace characters
  var="${var%"${var##*[![:space:]]}"}"   # remove trailing whitespace characters
  echo -n "$var"
}

__direnv_setup
