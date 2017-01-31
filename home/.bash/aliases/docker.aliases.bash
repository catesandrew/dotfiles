__docker_func_wrap () {
    local cur words cword prev
    _get_comp_words_by_ref -n =: cur words cword prev
    $1
}

# __git_complete gco _git_checkout
__docker_complete () {
    local wrapper="__docker_wrap${2}"
    eval "$wrapper () { __docker_func_wrap $2 ; }"
    complete -o bashdefault -o default -o nospace -F $wrapper $1 2>/dev/null \
        || complete -o default -o nospace -F $wrapper $1
}

__docker_c() {
	local fields=
	[ "${DOCKER_COMPLETION_TLS}" = yes ]
      fields='--tls'

    docker ${fields} "$@"
}

_completion_loader docker

# Docker Commands

# ------------------------------------
# Docker alias and function
# ------------------------------------

# Get latest container ID
dl() {
    __docker_c ps -l -q "$@"
}
__docker_complete dl _docker_ps

# Get process included stop container
dpa() {
    __docker_c ps -a "$@"
}
__docker_complete dpa _docker_ps

# Get container process
dps() {
    __docker_c ps "$@"
}
__docker_complete dps _docker_ps

# Attach to a running container
da() {
    __docker_c attach "$@"
}
__docker_complete da _docker_attach

# Remove one or more containers
drm() {
    __docker_c rm "$@"
}
__docker_complete drm _docker_rm

# Start one or more stopped containers
dstart() {
    __docker_c start "$@"
}
__docker_complete dstart _docker_start

# Stop one or more running containers
dstop() {
    __docker_c stop "$@"
}
__docker_complete dstop _docker_stop

# Stop and Remove all containers
drmf() {
    local _ps
    _ps="$(__docker_c ps -a -q)"

    echo "Stop and remove (all containers): $_ps"
    read -p "Are you sure? " -n 1 -r
    echo    # (optional) move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        __docker_c stop "$_ps" && __docker_c rm "$_ps"
    fi
}

# Build an image from a Dockerfile
dbuild() {
    __docker_c build "$@"
}
__docker_complete dbuild _docker_build

# Dockerfile build, e.g., $dbu tcnksm/test
dbu() {
    if [ $# -eq 0 ]; then
        local _name=$(basename "$PWD")
         __docker_c build --tag="$_name" .
    else
        __docker_c build --tag="$1" .
    fi

    # __docker_c build --tag="$1" .
}
__docker_complete dbu __docker_complete_image_repos_and_tags

# Create a new image from a container's changes
dci() {
    __docker_c commit "$@"
}
__docker_complete dci _docker_commit

# Copy files/folders between a container and the local filesystem
dcp() {
    __docker_c cp "$@"
}
__docker_complete dcp _docker_cp

ddaemon() {
    __docker_c daemon "$@"
}
__docker_complete ddaemon _docker_daemon

# Show docker disk usage, including space reclaimable by pruning
ddf() {
    __docker_c system df
}

# Inspect changes on a container's filesystem
ddi() {
    __docker_c diff "$@"
}
__docker_complete ddi _docker_diff

# Get real time events from the server
de() {
    __docker_c events "$@"
}
__docker_complete de _docker_events

# Run a command in a running container
dexec() {
    __docker_c exec "$@"
}
__docker_complete dexec _docker_exec

# enter into a running container
dent() {
    if [ $# -eq 0 ]; then
        local _name=$(basename "$PWD")
         __docker_c exec --interactive --tty "$_name" /bin/bash
    else
        __docker_c exec --interactive --tty "$1" /bin/bash
    fi

    # __docker_c exec --interactive --tty "$1" /bin/bash
}
__docker_complete dent __docker_complete_containers_running


# Export a container's filesystem as a tar archive
dexp() {
    __docker_c export "$@"
}
__docker_complete dexp _docker_export

# Show the history of an image
dh() {
    __docker_c history "$@"
}
__docker_complete dh _docker_history

# List images
dls() {
    __docker_c images "$@"
}
__docker_complete dls _docker_images

# Import the contents from a tarball to create a filesystem image
dimp() {
    __docker_c import "$@"
}
__docker_complete dimp _docker_import

# Get container IP
dip() {
    __docker_c inspect --format '{{ .NetworkSettings.IPAddress }}' "$@"
}
__docker_complete dip __docker_complete_containers_all

# Return low-level information on a container, image or task
di() {
    if [ $# -eq 0 ]; then
        local _name=$(basename "$PWD")
        __docker_c inspect "$_name"
    else
        __docker_c inspect "$@"
    fi
}
__docker_complete di _docker_inspect

# Kill one or more running containers
dkill() {
    __docker_c kill "$@"
}
__docker_complete dkill _docker_kill

# Load an image from a tar archive or STDIN
dload() {
    __docker_c load "$@"
}
__docker_complete dload _docker_load

# Log in to a Docker registry.
dlogin() {
    __docker_c login "$@"
}
__docker_complete dlogin _docker_login

# Log out from a Docker registry.
dlogout() {
    __docker_c logout "$@"
}
__docker_complete dlogout _docker_logout

# Fetch the logs of a container
dlogs() {
    __docker_c logs "$@"
}
__docker_complete dlogs _docker_logs

# Manage Docker networks
dnet() {
    __docker_c network "$@"
}
__docker_complete dnet _docker_network

# Manage Docker Swarm nodes
dnode() {
    __docker_c node "$@"
}
__docker_complete dnode _docker_node

# Pause all processes within one or more containers
dpause() {
    __docker_c pause "$@"
}
__docker_complete dpause _docker_pause

# List port mappings or a specific mapping for the container
dport() {
    __docker_c port "$@"
}
__docker_complete dport _docker_port

# Pull an image or a repository from a registry
dpull() {
    __docker_c pull "$@"
}
__docker_complete dpull _docker_pull

# Push an image or a repository to a registry
dpush() {
    __docker_c push "$@"
}
__docker_complete dpush _docker_push

# Rename a container
drename() {
    __docker_c rename "$@"
}
__docker_complete drename _docker_rename

# Restart a container
drestart() {
    __docker_c restart "$@"
}
__docker_complete drestart _docker_restart

# Remove one or more images
drmi() {
    __docker_c rmi "$@"
}
__docker_complete drmi _docker_rmi

# Remove image specified by $1
dri() {
    if [ $# -eq 0 ]; then
        local _name=$(basename "$PWD")
        __docker_c rmi "$_name"
    else
        __docker_c rmi "$@"
    fi
}
__docker_complete dri _docker_rmi

# Remove all untagged images
driu() {
   # This works by using rmi with a list of image ids. To get the image
   # ids we call docker images then pipe it to grep "^<none>". The grep
   # will filter it down to only lines with the value "<none>" in the
   # repository column. Then to extract the id out of the third column we
   # pipe it to awk "{print $3}" which will print the third column of each
   # line passed to it.
   local images
   images="$(__docker_c images | grep "^<none>" | awk '{print $3}')"
   # or
   # images="$(__docker_c images -q -f dangling=true)"

   echo "Remove all untagged images: $images"
   read -p "Are you sure? " -n 1 -r
   echo    # (optional) move to a new line
   if [[ $REPLY =~ ^[Yy]$ ]]; then
       __docker_c rmi "$images"
   fi
}

# Remove all images
drif() {
    local images
    images="$(__docker_c images -q)"

    echo "Remove all images: $images"
    read -p "Are you sure? " -n 1 -r
    echo    # (optional) move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        __docker_c rmi "$images"
    fi
}

# Run a command in a new container
drun() {
    if [ $# -eq 0 ]; then
        local _name=$(basename "$PWD")
         __docker_c run "$_name"
    else
        __docker_c run "$@"
    fi
}
__docker_complete drun _docker_run

# Run a command in a new container
dk() {
    __docker_c run "$@"
}
__docker_complete dk _docker_run

# Run deamonized container, e.g., $dkd base /bin/echo hello
dkd() {
    __docker_c run --detach --publish-all "$@"
}
__docker_complete dkd __docker_complete_images

# Run interactive container, e.g., $dki base /bin/bash
dki() {
    __docker_c run --interactive --tty --publish-all "$@"
}
__docker_complete dki __docker_complete_images

# run bash for any image
# dbash is particularly useful when diagnosing a failed `docker build`. Just
# dbash the last generated image and re-run the failed command
dbash() {
    if [ $# -eq 0 ]; then
        local _name=$(basename "$PWD")
        __docker_c run --rm --interactive --tty --env TERM=xterm --entrypoint /bin/bash "$_name"
    else
        __docker_c run --rm --interactive --tty --env TERM=xterm --entrypoint /bin/bash "$1"
    fi

    # __docker_c run --rm --interactive --tty --env TERM=xterm --entrypoint /bin/bash "$1"
}
__docker_complete dbash  __docker_complete_images

# run sh for any image
# dsh is particularly useful when diagnosing a failed `docker build`. Just
# dsh the last generated image and re-run the failed command
dsh() {
    if [ $# -eq 0 ]; then
        local _name=$(basename "$PWD")
        __docker_c run --rm --interactive --tty --env TERM=xterm --entrypoint sh "$_name"
    else
        __docker_c run --rm --interactive --tty --env TERM=xterm --entrypoint sh "$1"
    fi
}
__docker_complete dsh  __docker_complete_images

# Save one or more images to a tar archive (streamed to STDOUT by default)
dsave() {
    __docker_c save "$@"
}
__docker_complete dsave _docker_save

# Search the Docker Hub for images
dsearch() {
    __docker_c search "$@"
}
__docker_complete dsearch _docker_search

# Manage Docker services
dsrv() {
    __docker_c service "$@"
}
__docker_complete dsrv _docker_service

# Display a live stream of container(s) resource usage statistics
dstats() {
    __docker_c stats "$@"
}
__docker_complete dstats _docker_stats

dsudo() {
  if [ $# -eq 0 ]; then
    local _name=$(basename "$PWD")
    __docker_c run -it --rm --privileged --net=host --pid=host -v /:/host debian:sid chroot /host "$_name"
  else
    __docker_c run -it --rm --privileged --net=host --pid=host -v /:/host debian:sid chroot /host "$@"
  fi
}

dx11() {
  local name sound x11

  sound=(
	  --device /dev/snd
  )

  x11=(
	  -v /tmp/.X11-unix:/tmp/.X11-unix:ro
	  -e DISPLAY
	  --net host # in case of DISPLAY=localhost:*
	  -v ~/.Xauthority:/home/user/.Xauthority:ro
	  --ipc host # DevShm errors...
  )

  if [ $# -eq 0 ]; then
    _name=$(basename "$PWD")
    __docker_c run "${x11[@]}" "${sound[@]}" "$_name"
  else
    __docker_c run "${x11[@]}" "${sound[@]}" "$@"
  fi
}

# Manage Docker Swarm
dswarm() {
    __docker_c swarm "$@"
}
__docker_complete dswarm _docker_service

# Tag an image into a repository
dtag() {
    __docker_c tag "$@"
}
__docker_complete dtag _docker_tag

# Display the running processes of a container
dtap() {
    __docker_c tap "$@"
}
__docker_complete dtap _docker_tap

# Unpause all processes within one or more containers
dunpause() {
    __docker_c unpause "$@"
}
__docker_complete dunpause _docker_unpause

# Update configuration of one or more containers
dupdate() {
    __docker_c update "$@"
}
__docker_complete dupdate _docker_update

# Manage Docker volumes
dv() {
    __docker_c volume "$@"
}
__docker_complete dv _docker_volume

# Create a Volume
dvc() {
    __docker_c volume create "$@"
}
__docker_complete dvc _docker_volume_create

# Display detailed info on one or more volumes
dvi() {
    __docker_c volume inspect "$@"
}
__docker_complete dvi _docker_volume_inspect

# list volumes
dvls() {
    __docker_c volume ls "$@"
}
__docker_complete dvls _docker_volume_ls

dvcleanup() {
  __docker_c volume rm $(__docker_c volume ls -qf dangling=true)
}

# remove one or more volumes
dvrm() {
    __docker_c volume rm "$@"
}
__docker_complete dvrm _docker_volume_rm

# Block until a container stops, then print its exit code
dwait() {
    __docker_c wait "$@"
}
__docker_complete dwait _docker_wait

# Show all alias related docker
dalias() {
    alias | grep 'docker' | sed "s/^\([^=]*\)=\(.*\)/\1 => \2/"| sed "s/['|\']//g" | sort;
}

dclean() {
  local dockerInfo dockerRoot dockerPs dockerVolumes dockerUntaggedImages
  dockerInfo="$(__docker_c -D info)"
  dockerRoot="$(echo "$dockerInfo" | awk -F ': ' '$1 == "Docker Root Dir" { print $2; return }')"
  : "${dockerRoot:=/var/lib/docker}"

  dockerPs=( $(__docker_c ps -aq) )
  if [ ${#dockerPs[@]} -gt 0 ]; then
	  (
      echo "PS: "
      echo "${dockerPs[@]}"
		  # df -hT "$dockerRoot"
		  # df -hTi "$dockerRoot"
		  # __docker_c rm "${dockerPs[@]}" || true
		  # df -hT "$dockerRoot"
		  # df -hTi "$dockerRoot"
	  )
  fi

  dockerVolumes=( $(__docker_c volume ls -q) )
  if [ ${#dockerVolumes[@]} -gt 0 ]; then
	  (
      echo "VOLUMES:"
      echo "${dockerVolumes[@]}"
      echo ""
		  # df -hT "$dockerRoot"
		  # df -hTi "$dockerRoot"
		  # __docker_c volume rm "${dockerVolumes[@]}" || true
		  # df -hT "$dockerRoot"
		  # df -hTi "$dockerRoot"
	  )
  fi

  dockerUntaggedImages=( $(__docker_c images -q --filter 'dangling=true') )
  if [ ${#dockerUntaggedImages[@]} -gt 0 ]; then
	  (
      echo "UNTAGGED IMAGES:"
      echo "${dockerUntaggedImages[@]}"
      echo ""
		  # df -hT "$dockerRoot"
		  # df -hTi "$dockerRoot"
		  # __docker_c rmi "${dockerUntaggedImages[@]}" || true
		  # df -hT "$dockerRoot"
		  # df -hTi "$dockerRoot"
	  )
  fi
}

dhubtags() {
  local jqExpression self page repo nextPage
  jqExpression='.results[].name'
  self="dhubtags"

  usage() {
    cat <<-EOUSAGE

    usage: $self [options] repo [repo ...]

       ie: $self library/docker

           $self library/docker tianon/docker-master | sort -uV

           $self -j '.results[]' tianon/syncthing

           $self -j '.results[] | [ .name, (.full_size | tostring), .last_updated ] | join("\\t")' tianon/syncthing | sort -V | column -ts \$'\\t'

    options:
      -j='$jqExpression'
        modify the "j" expression used to extract results

EOUSAGE
  }

  if [ "$#" -eq 0 ]; then
    echo >&2 'error: expected at lease one "repo" argument'
    usage >&2
    return 1
  fi

  OPTIND=1
  while getopts "hj:" opt "$@"; do
    case $opt in
      j)
        jqExpression="$OPTARG";;
      h)
        usage >&2
        return 0
        ;;
      \?)
        echo "Invalid option: -$OPTARG" >&2
        usage;
        return 1
        ;;
      :)
        echo "Option -$OPTARG requires an argument." >&2
        return 1
        ;;
    esac
  done
  shift $((OPTIND-1))  #This tells getopts to move on to the next argument.

  _all() {
    repo="$1"; shift 1

    nextPage="https://hub.docker.com/v2/repositories/${repo}/tags/?page_size=100"
    while true; do
      page="$(curl -fsSL "$nextPage")"

      [ "$(echo "$page" | jq --raw-output '.results | length')" -gt 0 ] || break
      echo "$page" | jq --raw-output "$jqExpression"

      nextPage="$(echo "$page" | jq --raw-output '.next')"
      [ "$nextPage" != 'null' ] || break
    done
  }

  while [ "$#" -gt 0 ]; do
	  repo="$1"; shift 1
	  _all "$repo"
  done
}

dsshbg() {
  local dockerArgs before sawDash

  usage() {
    cat <<-EOUSAGE

 usage: dsshbg [docker args --] [ssh args]

        dsshbg --name ssh-some-server -- me@some.server -o ControlMaster=yes
          in ~/.ssh/config:
            Match all
                # use sockets if they're available
                ControlPath ~/.ssh/sockets/%r@%h-%p

        dsshbg --name docker-some-server --expose 2375 -- me@some.server -L 0.0.0.0:2375:/var/run/docker.sock
        docker -H tcp://docker-some-server.docker:2375 info

EOUSAGE
  }

  if [ "$#" -eq 0 ]; then
    echo >&2 'error: expected at lease one "repo" argument'
    usage >&2
    return 1
  fi

  dockerArgs=(
    -d #-it
    --restart always
    -v "$PWD:/host/$PWD"
    -w "/host/$PWD"
    -v "$HOME:/host/$HOME"
    -e HOME="/host/$HOME"
    -v "$HOME/.ssh:$HOME/.ssh" # uhh, ssh(1) is silly (doesn't respect HOME)
    # Docker reads /etc/passwd and /etc/group before mounting volumes
    -u "$(id -u):$(id -g)"
    $(id -G | xargs -n1 echo --group-add)
    -v /etc/passwd:/etc/passwd:ro
    -v /etc/group:/etc/group:ro
    -v /etc/localtime:/etc/localtime:ro
    -e HISTSIZE -e HISTFILE -e HISTFILESIZE -e HISTCMD -e HISTCONTROL
    --tmpfs /tmp
  )

  before=()
  sawDash=
  while [ "$#" -gt 0 ]; do
    i="$1"; shift
    if [ "$i" = '--' ]; then
      sawDash=1
      break
    fi
    before+=( "$i" )
  done

  if [ "$sawDash" ]; then
    dockerArgs+=( "${before[@]}" )
  else
    set -- "${before[@]}"
  fi

  exec __docker_c run "${dockerArgs[@]}" buildpack-deps:sid-scm ssh -N "$@"
}

dstale() {
  local containers
  IFS=$'\n'
  containers=( $(__docker_c ps --all --quiet --no-trunc) )
  unset IFS

  for container in "${containers[@]}"; do
	  name="$(__docker_c inspect --type container --format '{{.Name}}' "$container")"
	  name="${name#/}"
	  imageId="$(__docker_c inspect --type container --format '{{.Image}}' "$container")"
	  image="$(__docker_c inspect --type container --format '{{.Config.Image}}' "$container")"
	  imageImageId="$(__docker_c inspect --type image --format '{{.Id}}' "$image")"
	  if [ "$imageId" != "$imageImageId" ]; then
		  echo "- $name ($image)"
	  fi
  done
}
