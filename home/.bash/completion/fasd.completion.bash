if ! brew_contains_element "fasd" && \
    ! hash fasd 2>/dev/null; then
    exit 0
fi

# bash command mode completion
_fasd_bash_cmd_complete() {
  # complete command after "-e"
  local cur=${COMP_WORDS[COMP_CWORD]}
  [[ ${COMP_WORDS[COMP_CWORD-1]} == -*e ]] && \
    COMPREPLY=( $(compgen -A command $cur) ) && return
  # complete using default readline complete after "-A" or "-D"
  case ${COMP_WORDS[COMP_CWORD-1]} in
    -A|-D) COMPREPLY=( $(compgen -o default $cur) ) && return;;
  esac
  # get completion results using expanded aliases
  local RESULT=$( fasd --complete "$(alias -p $COMP_WORDS \
    2>> "/dev/null" | sed -n "\$s/^.*'\\(.*\\)'/\\1/p")
    ${COMP_LINE#* }" | while read -r line; do
      quote_readline "$line" 2>/dev/null || \
        printf %q "$line" 2>/dev/null  && \
        printf \\n
    done)
  local IFS=$'\n'; COMPREPLY=( $RESULT )
}
_fasd_bash_hook_cmd_complete() {
  for cmd in $*; do
    complete -F _fasd_bash_cmd_complete $cmd
  done
}