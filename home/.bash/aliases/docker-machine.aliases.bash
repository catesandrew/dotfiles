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

  docker-machine ${fields} 2>/dev/null "$@"
}

_completion_loader docker-machine

dm() {
  __docker_machine_c "$@"
}
__docker_machine_complete dm _docker_machine

# Print which machine is active
dma() {
  __docker_machine_c active "$@"
}
__docker_machine_complete dma _docker_machine_active

# Print the connection config for machine
dmc() {
  __docker_machine_c config "$@"
}
__docker_machine_complete dmc _docker_machine_config

# Create a machine
dmcreate() {
  __docker_machine_c create "$@"
}
__docker_machine_complete dmcreate _docker_machine_create

# Display the commands to set up the environment for the Docker client
dmenv() {
  __docker_machine_c env "$@"
}
__docker_machine_complete dmenv _docker_machine_env

# Inspect information about a machine
dmi() {
  __docker_machine_c inspect "$@"

}
__docker_machine_complete dm _docker_machine_inspect

# Get the IP address of a machine
dmip() {
  __docker_machine_c ip	"$@"
}
__docker_machine_complete dmip _docker_machine_ip

# Kill a machine
dmk() {
  __docker_machine_c kill "$@"
}
__docker_machine_complete dmk _docker_machine_kill

# ls			List machines
dmls() {
  __docker_machine_c ls "$@"
}
__docker_machine_complete dmls _docker_machine_ls

# Re-provision existing machines
dmp() {
  __docker_machine_c provision "$@"
}
__docker_machine_complete dmp _docker_machine_provision

# Restart a machine
dmrestart() {
  __docker_machine_c restart "$@"
}
__docker_machine_complete dmrestart _docker_machine_restart

# Remove a machine
dmrm() {
  __docker_machine_c rm "$@"
}
__docker_machine_complete dmrm _docker_machine_rm

# Log into or run a command on a machine with SSH.
dmssh() {
  __docker_machine_c ssh "$@"
}
__docker_machine_complete dmssh _docker_machine_ssh

# Copy files between machines
dmscp() {
  __docker_machine_c scp "$@"
}
__docker_machine_complete dmscp _docker_machine_scp

# Start a machine
dmstart() {
  __docker_machine_c start "$@"
}
__docker_machine_complete dmstart _docker_machine_start

# Get the status of a machine
dmst() {
  __docker_machine_c status "$@"
}
__docker_machine_complete dmst _docker_machine_status

# Stop a machine
dmstop() {
  __docker_machine_c stop "$@"
}
__docker_machine_complete dmstop _docker_machine_stop

# Get the URL of a machine
dmurl() {
  __docker_machine_c url "$@"
}
__docker_machine_complete dmurl _docker_machine_url

dmuse() {
  eval "$(docker-machine env "$@")"
  echo "Active machine: ${DOCKER_MACHINE_NAME}"
}
__docker_machine_complete dmuse _docker_machine_use
