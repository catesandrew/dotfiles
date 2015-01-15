cite 'about-alias'
about-alias 'docker'

__docker_func_wrap ()
{
    local cur words cword prev
    _get_comp_words_by_ref -n =: cur words cword prev
    $1
}

# __git_complete gco _git_checkout
__docker_complete ()
{
    local wrapper="__docker_wrap${2}"
    eval "$wrapper () { __docker_func_wrap $2 ; }"
    complete -o bashdefault -o default -o nospace -F $wrapper $1 2>/dev/null \
        || complete -o default -o nospace -F $wrapper $1
}

# Docker Commands

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
dstop() {
    if [ $# -eq 0 ] ; then
        docker stop $(docker ps -a -q);
    else
        docker stop $1;
    fi
}
__docker_complete dstop _docker_stop

# Remove all containers
drm() {
    if [ $# -eq 0 ] ; then
        docker rm $(docker ps -a -q);
    else
        docker rm $1;
    fi
}
__docker_complete drm _docker_rm

# Stop and Remove all containers
alias drmf='docker stop $(docker ps -a -q) && docker rm $(docker ps -a -q)'

# Remove all images
dri() {
    if [ $# -eq 0 ] ; then
        docker rmi $(docker images -q);
    else
        docker rmi $1;
    fi
}
__docker_complete dri _docker_rmi

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
dent() {
    docker exec -i -t $1 /bin/bash
}
__docker_complete dent _docker_exec

# run bash for any image
# dbash is particularly useful when diagnosing a failed `docker build`. Just
# dbash the last generated image and re-run the failed command
dbash() {
    docker run --rm -i -t -e TERM=xterm --entrypoint /bin/bash $1 
}
__docker_complete dbash __docker_image_repos

