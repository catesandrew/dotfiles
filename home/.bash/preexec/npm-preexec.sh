#
# npm-preexec.sh
#

# Avoid duplicate inclusion
if [[ "$__npm_imported" == "defined" ]]; then
    return 0
fi
__npm_imported="defined"

__npm_setup() {

    # check that we're using bash
    if [[ -n $BASH_VERSION ]]; then
        __npm_hook
    fi
}

__npm_hook() {
    # Hook into preexec and precmd functions
    if ! contains_element __npm_bash_preexec "${preexec_functions[@]}"; then
        preexec_functions+=(__npm_bash_preexec)
    fi

    if ! contains_element __npm_bash_precmd "${precmd_functions[@]}"; then
        precmd_functions+=(__npm_bash_precmd)
    fi
}

__npm_bash_preexec() {
  true
}

__npm_bash_precmd() {
  # Sanity check to make sure we have something to invoke our function with.
  if [ -z "$__bp_trimmed_arg" ]; then
    return
  fi

  local __node_cmd_i
  local __nm_dir

  IFS=' ' read -ra args <<< "$__bp_trimmed_arg"
  if [[ "${args[0]}" = 'npm' ]]; then
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
          tmutil addexclusion "${__nm_dir}"
          # prevent spotlight by creating an empty file .metadata_never_index inside the folder
          # (eg with touch folder/.metadata_never_index)
          touch "${__nm_dir}/.metadata_never_index"
        fi
      fi

      unset __node_cmd_i
      unset __nm_dir
  fi

  # for arg in "${args[1]}"; do
  #   if [ "$arg" = "install" ]; then
  #     __node_modules_found="yes"
  #   fi
  # done
}

__npm_setup
