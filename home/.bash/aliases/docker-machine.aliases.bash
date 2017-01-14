__docker_machine_func_wrap () {
  local cur words cword prev
  _get_comp_words_by_ref -n =: cur words cword prev
  $1
}

# __git_complete gco _git_checkout
__docker_machine_complete () {
  local wrapper="__docker_machine_wrap${2}"
  eval "$wrapper () { __docker_machine_func_wrap $2; }"
  complete -o bashdefault -o default -o nospace -F "$wrapper" "$1" 2>/dev/null \
    || complete -o default -o nospace -F "$wrapper" "$1"
}

__docker_machine_c() {
  local fields=
  # [ "${DOCKER_COMPLETION_TLS}" = yes ]
  # fields='--tls'

  docker-machine ${fields} "$@"
}

_completion_loader docker-machine

dmachine() {
  __docker_machine_c "$@"
}
__docker_machine_complete dmachine _docker_machine

dm() {
  __docker_machine_c "$@"
}
__docker_machine_complete dm _docker_machine

# Get container process
dcps() {
  __docker_machine_c ps "$@"
}
__docker_machine_complete dcps _docker_machine_ps
