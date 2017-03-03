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

dcomp() {
  __docker_compose_c "$@"
}
__docker_compose_complete dcomp _docker_compose

# Get container process
dcps() {
  __docker_compose_c ps "$@"
}
__docker_compose_complete dcps _docker_compose_ps

# Build or rebuild services
dcbuild() {
  __docker_compose_c build "$@"
}
__docker_compose_complete dcbuild _docker_compose_build

# Generate a Docker bundle from the Compose file
bundle() {
  __docker_compose_c bundle "$@"
}
__docker_compose_complete dcbuild _docker_compose_build

# Validate and view the compose file
dcconfig() {
  __docker_compose_c config "$@"
}
__docker_compose_complete dcconfig _docker_compose_config

# Create services
dccreate() {
  __docker_compose_c create "$@"
}
__docker_compose_complete dccreate _docker_compose_create

# Stop and remove containers, networks, images, and volumes
dcdown() {
  __docker_compose_c down "$@"
}
__docker_compose_complete dcdown _docker_compose_down

# Receive real time events from containers
dcevents() {
  __docker_compose_c events "$@"
}
__docker_compose_complete dcevents _docker_compose_events

# Execute a command in a running container
dcexec() {
  __docker_compose_c exec "$@"
}
__docker_compose_complete dcexec _docker_compose_exec

# Get help on a command
dchelp() {
  __docker_compose_c help "$@"
}
__docker_compose_complete dchelp _docker_compose_help

# Kill containers
dckill() {
  __docker_compose_c kill "$@"
}
__docker_compose_complete dckill _docker_compose_kill

# View output from containers
dclogs() {
  __docker_compose_c logs "$@"
}
__docker_compose_complete dclogs _docker_compose_logs

# Pause services
dcpause() {
  __docker_compose_c pause "$@"
}
__docker_compose_complete dcpause _docker_compose_pause

# Print the public port for a port binding
dcport() {
  __docker_compose_c port "$@"
}
__docker_compose_complete dcport _docker_compose_port

# List containers
dcps() {
  __docker_compose_c ps "$@"
}
__docker_compose_complete dcps _docker_compose_ps

# Pull service images
dcpull() {
  __docker_compose_c pull "$@"
}
__docker_compose_complete dcpull _docker_compose_pull

# Push service images
dcpush() {
  __docker_compose_c push "$@"
}
__docker_compose_complete dcpush _docker_compose_push

# Restart services
dcrestart() {
  __docker_compose_c restart "$@"
}
__docker_compose_complete dcrestart _docker_compose_restart

# Remove stopped containers
dcrm() {
  __docker_compose_c rm "$@"
}
__docker_compose_complete dcrm _docker_compose_rm

# Run a one-off command
dcrun() {
  __docker_compose_c run "$@"
}
__docker_compose_complete dcrun _docker_compose_run

# Set number of containers for a service
dcscale() {
  __docker_compose_c scale "$@"
}
__docker_compose_complete dcscale _docker_compose_scale

# Start services
dcstart() {
  __docker_compose_c start "$@"
}
__docker_compose_complete dcstart _docker_compose_start

# Stop services
dcstop() {
  __docker_compose_c stop "$@"
}
__docker_compose_complete dcstop _docker_compose_stop

# Display the running processes
dctop() {
  __docker_compose_c top "$@"
}
__docker_compose_complete dctop _docker_compose_top

# Unpause services
dcunpause() {
  __docker_compose_c unpause "$@"
}
__docker_compose_complete dcunpause _docker_compose_unpause

# Create and start containers
dcup() {
  __docker_compose_c up "$@"
}
__docker_compose_complete dcup _docker_compose_up

# Show the Docker-Compose version information
dcversion() {
  __docker_compose_c version "$@"
}
