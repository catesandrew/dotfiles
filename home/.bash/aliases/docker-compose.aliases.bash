__docker_compose_func_wrap () {
  local cur words cword prev
  _get_comp_words_by_ref -n =: cur words cword prev
  $1
}

# __git_complete gco _git_checkout
__docker_compose_complete () {
  local wrapper="__docker_compose_wrap${2}"
  eval "$wrapper () { __docker_compose_func_wrap $2; }"
  complete -o bashdefault -o default -o nospace -F "$wrapper" "$1" 2>/dev/null \
    || complete -o default -o nospace -F "$wrapper" "$1"
}

__docker_compose_c() {
  local fields=
  [ "${DOCKER_COMPLETION_TLS}" = yes ]
  fields='--tls'

  docker-compose ${fields} "$@"
}

_completion_loader docker-compose

dcompose() {
  __docker_compose_c "$@"
}
__docker_compose_complete dcompose _docker_compose

# Get container process
dcps() {
  __docker_compose_c ps "$@"
}
__docker_compose_complete dcps _docker_compose_ps
