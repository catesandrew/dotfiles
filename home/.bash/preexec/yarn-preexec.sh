#
# yarn-preexec.sh
#

# Avoid duplicate inclusion
if [[ "$__yarn_imported" == "defined" ]]; then
    return 0
fi
__yarn_imported="defined"

__yarn_setup() {
    # check that we're using bash
    if [[ -n $BASH_VERSION ]]; then
        __yarn_hook
    fi
}

__yarn_hook() {
    # Hook into preexec and precmd functions
    if ! contains_element __yarn_bash_preexec "${preexec_functions[@]}"; then
        preexec_functions+=(__yarn_bash_preexec)
    fi

    if ! contains_element __yarn_bash_precmd "${precmd_functions[@]}"; then
        precmd_functions+=(__yarn_bash_precmd)
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
#   __YARN_PWD The directory this command is being executed in
#   __YARN_SAVE_COMMAND The command that is being executed and to be saved.
#
# Arguments:
#  $1 The command just entered, about to be executed.
#
__yarn_bash_preexec() {
  __YARN_PWD="$PWD"
  __YARN_SAVE_COMMAND="$1"
}

__yarn_bash_precmd() {
  # Set this initially to properly catch the exit status.
  __YARN_EXIT_STATUS="$?"

  local command="$__YARN_SAVE_COMMAND"

  # Check if we need to process a command. If so, unset it as it will be
  # processed and saved.
  if [[ -n "$__YARN_SAVE_COMMAND" ]]; then
    unset __YARN_SAVE_COMMAND
  else
    return 0
  fi

  (__yarn_process_command "$command"&) 2>&1
}

#
# Send our command to the server if everything
# looks good.
#
# @param A trimmed command from the command line
#
__yarn_process_command() {
  local yarn_command
  yarn_command=$(__yarn_trim_whitespace "$1")

  # Sanity empty check
  if [[ -z "$yarn_command" ]]; then
    return 0;
  fi;

  # Check to make sure yarn is still installed. Otherwise, this will
  # simply fail and spam the user that files dont exist.
  # if ! type "yarn" &> /dev/null; then
  #   return 0;
  # fi;

  local __node_cmd_i
  local __nm_dir
  local process_id=$$
  local working_directory="$__YARN_PWD"
  local exit_status="$__YARN_EXIT_STATUS"

  IFS=' ' read -ra args <<< "$yarn_command"
  if [[ "${args[0]}" = 'yarn' ]]; then
      if [ "${args[1]}" = "install" ] || [ "${args[1]}" = "i" ]; then
        __node_cmd_i="yes"

        if [ ${#args[@]} -gt 2 ]; then
          while getopts ":g-:" optchar "${args[@]:2}"; do
            case "${optchar}" in
              -)
                case "${OPTARG}" in
                  global)
                    unset __node_cmd_i
                    ;;
                esac;;
              g)
                unset __node_cmd_i
                ;;
            esac
          done
        fi
      fi

      if [ -n "${__node_cmd_i}" ]; then
        # exclude the node_modules directory from time machine
        __nm_dir="$(readlink -f "node_modules")"
        if [ -d "$__nm_dir" ]; then
          (tmutil addexclusion "${__nm_dir}"&)
          # prevent spotlight by creating an empty file .metadata_never_index inside the folder
          # (eg with touch folder/.metadata_never_index)
          (touch "${__nm_dir}/.metadata_never_index"&)
        fi
      fi

      unset __node_cmd_i
      unset __nm_dir
  fi
}

__yarn_trim_whitespace() {
  local var=$@
  var="${var#"${var%%[![:space:]]*}"}"   # remove leading whitespace characters
  var="${var%"${var##*[![:space:]]}"}"   # remove trailing whitespace characters
  echo -n "$var"
}

__yarn_setup
