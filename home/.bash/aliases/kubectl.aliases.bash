# This command is used ALOT both below and in daily life
alias k=kubectl
complete -F _complete_alias k

alias ka='k apply'
complete -F _complete_alias ka

alias kc='k config'
complete -F _complete_alias kc

# Manage configuration quickly to switch contexts between local, dev ad staging.
alias kcuc='k config use-context'
complete -F _complete_alias kcuc

alias kcsc='k config set-context'
complete -F _complete_alias kcsc

alias kcdc='k config delete-context'
complete -F _complete_alias kcdc

alias kccc='k config current-context'
complete -F _complete_alias kccc

alias kcp='k copy'
complete -F _complete_alias kcp

alias kcr='k create'
complete -F _complete_alias kcr

alias kd='k describe'
complete -F _complete_alias kd

alias ked='k edit'
complete -F _complete_alias ked

alias kex='k exec'
complete -F _complete_alias kex

# Drop into an interactive terminal on a container
alias kexti='kex -ti'
complete -F _complete_alias kexti

alias kep='k explain'
complete -F _complete_alias kep

alias keo='k expose'
complete -F _complete_alias keo

alias kg='k get'
complete -F _complete_alias kg

# Pod management.
alias kgp='kg pods'

alias kls='kg'
complete -F _complete_alias kls

alias kl='k logs'
complete -F _complete_alias kl

alias ko='k options'
complete -F _complete_alias ko

alias kr='k replace'
complete -F _complete_alias kr

alias krm='k delete'
complete -F _complete_alias krm

alias ks='k set'
complete -F _complete_alias ks

alias kx='kex'
complete -F _complete_alias kx
function kssh() { ssh -A core@"$(kg node -o wide | grep "$@" | awk '{ print $4}')"; }
function kxsh() { kex -ti "$@" sh; }
function kxbash() { kex -ti "$@" bash; }

alias kv='k version'
complete -F _complete_alias kv

# # alias kgp='kubectl get pod -o wide'
# alias klp='k logs pods'
# alias kep='k edit pods'
# alias kdp='k describe pods'
# # alias kdp='kubectl describe pod'
# alias kdelp='k delete pods'

# # Service management.
# alias kgs='k get svc'
# # alias kgs='kubectl get service -o wide'
# alias kes='k edit svc'
# alias kds='k describe svc'
# # alias kds='kubectl describe service'
# alias kdels='k delete svc'

# # Secret management
# alias kgsec='k get secret'
# alias kdsec='k describe secret'
# alias kdelsec='k delete secret'

# # Deployment management.
# alias kgd='k get deployment'
# # alias kgd='kubectl get deployment -o wide'
# alias ked='k edit deployment'
# alias kdd='k describe deployment'
# # alias kdd='kubectl describe deployment'
# alias kdeld='k delete deployment'
# alias ksd='k scale deployment'
# alias krsd='k rollout status deployment'

# # Rollout management.
# alias kgrs='k get rs'
# alias krh='k rollout history'
# alias kru='k rollout undo'

# alias klf='kubectl logs --tail=200  -f'
# alias kgn='kubectl get node -o wide'
# alias kdf='kubectl delete -f'
# alias kaf='kubectl apply -f'
# alias kci='kubectl cluster-info'

# function kpp() {
#   kgp | grep "$@" | head -1 | awk '{ print $1}' | xargs -i kubectl port-forward {} 8080:8080 > /dev/null 2>&1 &
#   sleep 2;
#   xdg-open http://localhost:8080/__health > /dev/null 2>&1;
# }

# function kfp() { kubectl get pod -o wide| grep "$@"; }
# function kfs() { kubectl get service -o wide| grep "$@"; }
# function kfd() { kubectl get deployment -o wide | grep "$@"; }
# function kres() {
#   echo "Scaling $1"
#   desired_replicas=$(kubectl get deployments --selector=app="$1" -o jsonpath='{$.items[0].spec.replicas}');
#   echo "Desired nf or replicas: $desired_replicas";
#         echo "Scaling deployment $1 down to 0 replicas...";
#         kubectl scale --replicas=0 deployments/$1;
#   current_replicas=$(kubectl get deployments --selector=app="$1" -o jsonpath='{$.items[0].status.availableReplicas}')
#   while [ ! -z "$current_replicas" ]; do
#     sleep 1;
#           current_replicas=$(kubectl get deployments --selector=app="$1" -o jsonpath='{$.items[0].status.availableReplicas}')
#   done;
#   echo "Scaling deployment $1 up to $desired_replicas replicas...";
#   kubectl scale --replicas=$desired_replicas deployments/$1;
#   if [ "$2" == "skipCheck" ]; then
#     echo "Skipping healthchecks"
#     return
#   fi

#   default_sleep=10
#   initial_sleep=$(kubectl get deployments --selector=app=$1 -o jsonpath='{$.items[0].spec.template.spec.containers[0].readinessProbe.initialDelaySeconds}')
#   initial_sleep=${initial_sleep:-10}
#   echo "Waiting $initial_sleep seconds for the first readiness probe check..."
#         sleep $initial_sleep
#     period_sleep=$(kubectl get deployments --selector=app=$1 -o jsonpath='{$.items[0].spec.template.spec.containers[0].readinessProbe.periodSeconds}')
#   period_sleep=${period_sleep:-10}
#   while [ "$current_replicas" != "$desired_replicas" ]; do
#     echo "Waiting $period_sleep seconds until checking the node count"
#     sleep $period_sleep
#     current_replicas=$(kubectl get deployments --selector=app=$1 -o jsonpath='{$.items[0].status.availableReplicas}')
#                 current_replicas=${current_replicas:-0}
#                 echo "Current nr of replicas: $current_replicas"
#         done;
#   echo "$1 restarted"
# }
# function kgnt() { for machine in $(kcgn | tail -n +2 | awk '{ print $1 }' ); do echo -n "${machine}: "; echo $(kc describe node $machine | grep -i taints); done | sort -k 2; }
# function kstat() {
#   for node in  $(kubectl get nodes | tail -n +2 | awk '{print $1}'); do
#     echo $node
#     echo -e "$(kubectl describe node $node | grep -A 4 "Allocated resources")\n";
#   done
# }
# function kreach(){
#   for public_ip in  $(kubectl get nodes -o wide | tail -n +2 | awk '{print $4}'); do
#                 echo $public_ip
#                 echo -e "$(ssh core@$public_ip date)\n"
#         done
# }
# function kready() {
#         for node in  $(kubectl get nodes | tail -n +2 | awk '{print $1}'); do
#                 echo $node
#                 echo -e "$(kubectl describe node $node | grep  "Ready")\n";
#         done
# }
