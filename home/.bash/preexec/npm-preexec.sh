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

  local node_modules_found

  IFS=' ' read -ra args <<< "$__bp_trimmed_arg"
  if [[ "${args[0]}" = 'npm' ]]
  then
    for arg in "${args[1]}"; do
      if [ "$arg" = "install" ]; then
        node_modules_found="yes"
      fi
    done

    if [[ -n "${node_modules_found}" ]]
    then
      # exclude the node_modules directory from time machine
      local __nm=$(readlink -f "node_modules") && tmutil addexclusion "$__nm"
    fi
    node_modules_found=""
  fi
}

__npm_setup
