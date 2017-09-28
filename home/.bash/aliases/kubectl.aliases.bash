__kubectl_func_wrap () {
  local cur words cword prev
  _get_comp_words_by_ref -n =: cur words cword prev

  $1
  __handle_word
  if [ $# -eq 2 ]; then
    $2
  fi
}

__kubectl_complete () {
  if [ $# -eq 3 ]; then
    local wrapper="__kubectl_wrap${2}_${3}"
    eval "$wrapper () { __kubectl_func_wrap $2 $3 ; }"
  else
    local wrapper="__kubectl_wrap${2}"
    eval "$wrapper () { __kubectl_func_wrap $2 ; }"
  fi

  complete -o bashdefault -o default -o nospace -F "$wrapper" "$1" 2>/dev/null \
    || complete -o default -o nospace -F "$wrapper" "$1"
}

__kubectl_get_svc() {
  __kubectl_parse_get "svc"
}

__kubectl_get_deploy() {
  __kubectl_parse_get "deploy"
}

__kubectl_get_node() {
  __kubectl_parse_get "node"
}

__kubectl_get_pod() {
  __kubectl_parse_get "pod"
}

__kubectl_get_ns() {
  __kubectl_parse_get "namespace"
}

__kubectl_get_ep() {
  __kubectl_parse_get "endpoints"
}

_completion_loader kubectl

## This command is used ALOT both below and in daily life
alias k=kubectl
complete -F _complete_alias k

## Apply
alias ka='k apply'
complete -F _complete_alias ka

## Cluster Info

### * Get a list of services started on a cluster by kube system with the
### cluster-info command
alias kci='k cluster-info'

## Config
alias kc='k config'
complete -F _complete_alias kc

## * Check the location and credentials known about with this command:
alias kcv='kc view'

# Get the public IP address of one of your nodes. Either, `kubectl
# cluster-info`, or if you are using Google Compute Engine instances, you
# can use the `gcloud compute instances list` command to see the public
# addresses of your nodes.


## Manage configuration quickly to switch contexts between local, dev ad staging.
alias kcuc='k config use-context'
complete -F _complete_alias kcuc

alias kcsc='k config set-context'
complete -F _complete_alias kcsc

alias kcdc='k config delete-context'
complete -F _complete_alias kcdc

alias kccc='k config current-context'
complete -F _complete_alias kccc

### Setting the namespace preference

### You can permanently save the namespace for
### all subsequent kubectl commands in that context.
ksetns() {
  if [ $# -eq 0 ]; then
    kc set-context $(kc current-context) --namespace='default'
  else
    kc set-context $(kc current-context) --namespace="$@"
  fi
}

### Validate it
kgetns() {
  kc view | grep namespace | awk '{ print $2 }';
}

## Copy
alias kcp='k copy'
complete -F _complete_alias kcp

## Create
alias kcr='k create'
complete -F _complete_alias kcr

## Delete
alias krm='k delete'
complete -F _complete_alias krm

alias krmp='krm pod'
alias krms='krm svc'
alias krmd='krm deployment'
alias krmsec='krm secret'

## Describe


function kd() {
  # alias kd='k describe'
  # complete -F _complete_alias kd
  k describe "$@"
}
__kubectl_complete kd _kubectl_describe

alias kdsec='kd secret'

#### describe namespaces
function kdns() {
  kd namespace "$@"
}
__kubectl_complete kdns _kubectl_describe __kubectl_get_ns

#### describe pods
function kdp() {
  kd pod "$@"
}
__kubectl_complete kdp _kubectl_describe __kubectl_get_pod

#### describe endpoints
function kdep() {
  kd endpoints "$@"
}
__kubectl_complete kdep _kubectl_describe __kubectl_get_ep

#### describe nodes
function kdn() {
  kd node "$@"
}
__kubectl_complete kdn _kubectl_describe __kubectl_get_node

#### describe services
function kds() {
  kd svc "$@"
}
__kubectl_complete kds _kubectl_describe __kubectl_get_svc

#### describe deployments
function kdd() {
  kd deploy "$@"
}
__kubectl_complete kdd _kubectl_describe __kubectl_get_deploy

## Edit
alias ked='k edit'
complete -F _complete_alias ked

alias kedp='ked pod'
alias keds='ked svc'
alias kedd='ked deployment'

## Exec
alias kex='k exec'
complete -F _complete_alias kex

### Drop into an interactive terminal on a container
alias kexti='kex -ti'
complete -F _complete_alias kexti

alias kx='kex'
complete -F _complete_alias kx
function kssh() { ssh -A core@"$(kg node -o wide | grep "$@" | awk '{ print $4}')"; }
function kxsh() { kex -ti "$@" sh; }
function kxbash() { kex -ti "$@" bash; }
function kxbusy() {
  if k get pod --selector run=busybox 2>/dev/null | grep busybox > /dev/null 2>&1; then
    kxsh busybox
  else
    k run -ti busybox --image=busybox --generator="run-pod/v1" "$@";
  fi
}

## Explain
alias kep='k explain'
complete -F _complete_alias kep

## Expose
alias keo='k expose'
complete -F _complete_alias keo

## Get
function kg() {
  # alias kg='k get -o wide'
  # complete -F _complete_alias kg
  k get -o wide "$@"
}
__kubectl_complete kg _kubectl_get

function kgj() {
  k get -o json "$@" | jq
}
__kubectl_complete kgj _kubectl_get

#### get namespaces
function kgns() {
  kg namespace "$@"
}
__kubectl_complete kgns _kubectl_get __kubectl_get_ns

function kgjns() {
  kgj namespace "$@"
}
__kubectl_complete kgjns _kubectl_get __kubectl_get_ns

#### get pods
function kgp() {
  kg pod "$@"
}
__kubectl_complete kgp _kubectl_get __kubectl_get_pod

function kgjp() {
  kgj pod "$@"
}
__kubectl_complete kgjp _kubectl_get __kubectl_get_pod

#### get endpoints
function kgep() {
  kg endpoints "$@"
}
__kubectl_complete kgep _kubectl_get __kubectl_get_ep

function kgjep() {
  kgj endpoints "$@"
}
__kubectl_complete kgjep _kubectl_get __kubectl_get_ep

#### get nodes
function kgn() {
  kg node "$@"
}
__kubectl_complete kgn _kubectl_get __kubectl_get_node

function kgjn() {
  kgj node "$@"
}
__kubectl_complete kgjn _kubectl_get __kubectl_get_node

#### get services
function kgs() {
  kg svc "$@"
}
__kubectl_complete kgs _kubectl_get __kubectl_get_svc

function kgjs() {
  kgj svc "$@"
}
__kubectl_complete kgjs _kubectl_get __kubectl_get_svc

#### get deployments
function kgd() {
  kg deploy "$@"
}
__kubectl_complete kgd _kubectl_get __kubectl_get_deploy

function kgjd() {
  kgj deploy "$@"
}
__kubectl_complete kgjd _kubectl_get __kubectl_get_deploy

### Rollout management.
alias kgrs='kg rs'
alias kgsec='kg secret'

kls() {
  if [ $# -eq 0 ]; then
    # kg -o wide all

    for cmd in pods services deployments replicasets jobs endpoints ingress; do
      if ! k get $cmd 2>&1 | grep 'No resources found' > /dev/null 2>&1; then
        echo ""
        echo "$cmd:"
        k get -o wide $cmd
      fi
    done

    # if ! k get endpoints 2>&1 | grep 'No resources found' > /dev/null 2>&1; then
    #   echo ""
    #   k get endpoints
    # fi


    # if ! k get ingress 2>&1 | grep 'No resources found' > /dev/null 2>&1; then
    #   echo ""
    #   k get ingress
    # fi
  else
    kg -o wide "$@"
  fi
}

kll() {
  if [ $# -eq 0 ]; then
    kg -o yaml all
  else
    kg -o yaml "$@"
  fi
}

## Logs
alias kl='k logs'
complete -F _complete_alias kl

alias klp='kl pods'
alias klf='kl --tail=200 -f'
complete -F _complete_alias klf

alias klogs='kl'
complete -F _complete_alias klogs

## Options
alias ko='k options'
complete -F _complete_alias ko

## Run
alias krun='k run'
complete -F _complete_alias krun

## Replace
alias krp='k replace'
complete -F _complete_alias krp

## Rollout
alias kro='k rollout'
complete -F _complete_alias kro

### Rollout management.
alias krosd='kro status deployment'
alias kroh='kro history'
alias krou='kro undo'

## Scale
alias ksc='k scale'
complete -F _complete_alias ksc

alias kscd='ksc deployment'

## Set
alias kset='k set'
complete -F _complete_alias kset

## Version
alias kv='k version'
complete -F _complete_alias kv

function oldkpp() {
  kgp | \grep "$@" | head -1 | awk '{ print $1 }' | xargs -i k port-forward {} 8080:8080 > /dev/null 2>&1 &
  sleep 2;

  if hash xdg-open 2>/dev/null; then
    xdg-open http://localhost:8080/__health > /dev/null 2>&1;
  elif hash open 2>/dev/null; then
    open http://localhost:8080/__health > /dev/null 2>&1;
  fi
}

function kstat() {
  for node in  $(kgn | tail -n +2 | awk '{ print $1}'); do
    echo "node: $node"
    echo -e "$(kubectl describe node $node | grep -A 4 "Allocated resources")\n";
  done
}

function kreach(){
  for public_ip in  $(kgn -o wide | tail -n +2 | awk '{ print $5 }'); do
    echo "public ip: $public_ip"
    echo -e "$(ssh "core@${public_ip}" date)\n"
  done
}

function kready() {
  for node in  $(kgn | tail -n +2 | awk '{ print $1 }'); do
    echo "node: $node"
    echo -e "$(kdn "${node}" | grep  "Ready")\n";
  done
}

# logs for pod
# function klp () {
#   local r="$1" p
#   [[ $PAGER ]] || PAGER=more
#   # match full pod name, pod in replica, or pod in deployment
#   for p in $(kg pods | awk "\$1 == \"$r\" || \$1 ~ /^$r-[a-z0-9]{5}$/ || \$1 ~ /^$r-[0-9]{1,10}-[a-z0-9]{5}$/ { print \$1 }"); do
#     echo "===> $p <==="
#     k logs "$p" | $PAGER
#   done
# }

# ssh into each host
function kssh () {
  local conf="ssh_config"
  local h
  for h in $(awk '/^Host / { print $2 }' "$conf"); do
    echo "$h"
    ssh -F "$conf" "$h" "$@"
  done
}

function kproxy() {
  local APISERVER=$(kcv | \grep server | cut -f 2- -d ":" | tr -d " ")
  local TOKEN=$(kd secret $(kg secrets | \grep default | cut -f1 -d ' ') | grep -E '^token' | cut -f2 -d':' | tr -d '\t')
  curl $APISERVER/api --header "Authorization: Bearer $TOKEN" --insecure
}

### Run some pods
# krun hostnames --image=gcr.io/google_containers/serve_hostname --labels=app=hostnames --port=9376 --replicas=3

### Confirm your Pods are running
# kg pod --selector app=hostnames

### Is there a hostname (Run from another Pod)?
# wget -qO- hostnames

### First thing to check is whether that Service actually exists
# kg svc hostnames

### So we have a culprit, let’s create the Service
# keo deployment hostnames --port=80 --target-port=9376

### Does the Service work by DNS (Run from another Pod)?
# nslookup hostnames

### Does any Service exist in DNS (Run from another Pod)?
# nslookup kubernetes.default

### Does the Service have any Endpoints?
# kg endpoints
# kg endpoints hostnames
