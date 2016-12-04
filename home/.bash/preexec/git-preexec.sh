#
# git-preexec.sh
#

# Avoid duplicate inclusion
if [[ "$__git_imported" == "defined" ]]; then
    return 0
fi
__git_imported="defined"

__git_setup() {
    # check that we're using bash
    if [[ -n $BASH_VERSION ]]; then
        __git_hook
    fi
}

__git_hook() {
    # Hook into preexec and precmd functions
    if ! contains_element __git_bash_preexec "${preexec_functions[@]}"; then
        preexec_functions+=(__git_bash_preexec)
    fi

    if ! contains_element __git_bash_precmd "${precmd_functions[@]}"; then
        precmd_functions+=(__git_bash_precmd)
    fi
}

__git_bash_preexec() {
  true
}

__git_bash_precmd() {
  # Sanity check to make sure we have something to invoke our function with.
  if [ -z "$__bp_trimmed_arg" ]; then
    return
  fi

  local __git_found
  local __git_dir

  IFS=' ' read -ra args <<< "$__bp_trimmed_arg"
  if [[ "${args[0]}" = 'git' ]]
  then
    for arg in ${args[1]}; do
      if [ "$arg" = "init" ]; then
        __git_found="yes"
      fi
    done

    if [[ -n "${__git_found}" ]]
    then
      # exclude the node_modules directory from time machine
      __git_dir=$(readlink -f .git)
      tmutil addexclusion "${__git_dir}"
      # prevent spotlight by creating an empty file .metadata_never_index
      touch "${__git_dir}/.metadata_never_index"
    fi

    unset __git_found
    unset __git_dir
  fi
}

__git_setup
