cite 'about-alias'
about-alias 'docker'

# Docker Commands

# One liner to stop / remove all of Docker containers:
# alias dsa='docker stop $(docker ps -a -q)'
# alias dra='docker rm $(docker ps -a -q)'

# ------------------------------------
# Docker alias and function
# ------------------------------------

# Get latest container ID
alias dl="docker ps -l -q"

# Get container process
alias dps="docker ps"

# Get process included stop container
alias dpa="docker ps -a"

# Get images
alias di="docker images"

# Get container IP
alias dip="docker inspect --format '{{ .NetworkSettings.IPAddress }}'"

# Run deamonized container, e.g., $dkd base /bin/echo hello
alias dkd="docker run -d -P"

# Run interactive container, e.g., $dki base /bin/bash
alias dki="docker run -i -t -P"

# Stop all containers
dstop() { docker stop $(docker ps -a -q); }

# Remove all containers
drm() { docker rm $(docker ps -a -q); }

# Stop and Remove all containers
alias drmf='docker stop $(docker ps -a -q) && docker rm $(docker ps -a -q)'

# Remove all images
dri() { docker rmi $(docker images -q); }

# Dockerfile build, e.g., $dbu tcnksm/test
dbu() { docker build -t=$1 .; }

# Show all alias related docker
dalias() { alias | grep 'docker' | sed "s/^\([^=]*\)=\(.*\)/\1 => \2/"| sed "s/['|\']//g" | sort; }

# http://viget.com/extend/how-to-use-docker-on-os-x-the-missing-guide
docker-ip() {
    boot2docker ip 2> /dev/null
}

docker2hostfile() {
    echo $(docker-ip) dockerhost | tee -a "${HOME}/Library/Gas Mask/Local/Docker.hst"
}

# enter into a running container

function dent {
docker exec -i -t $1 /bin/bash
}
complete -F _docker_exec dent

# run bash for any image
function dbash {
docker run --rm -i -t -e TERM=xterm --entrypoint /bin/bash $1 
}
complete -F _docker_images dbash

# dbash is particularly useful when diagnosing a failed `docker build`. Just
# dbash the last generated image and re-run the failed command
