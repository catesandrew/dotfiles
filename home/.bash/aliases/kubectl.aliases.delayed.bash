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

# __kubectl_get_svc() {
#   __kubectl_parse_get "svc"
# }

# __kubectl_get_deploy() {
#   __kubectl_parse_get "deploy"
# }

# __kubectl_get_node() {
#   __kubectl_parse_get "node"
# }

# __kubectl_get_ns() {
#   __kubectl_parse_get "namespace"
# }

# __kubectl_get_ep() {
#   __kubectl_parse_get "endpoints"
# }

__kubectl_get_csr() {
  __kubectl_parse_get "certificatesigningrequests"
}

__kubectl_get_cs() {
  __kubectl_parse_get "componentstatuses"
}

__kubectl_get_cm() {
  __kubectl_parse_get "configmaps"
}

__kubectl_get_crd() {
  __kubectl_parse_get "customresourcedefinition"
}

__kubectl_get_ds() {
  __kubectl_parse_get "daemonsets"
}

__kubectl_get_deploy() {
  __kubectl_parse_get "deployments"
}

__kubectl_get_ep() {
  __kubectl_parse_get "endpoints"
}

__kubectl_get_ev() {
  __kubectl_parse_get "events"
}

__kubectl_get_hpa() {
  __kubectl_parse_get "horizontalpodautoscalers"
}

__kubectl_get_ing() {
  __kubectl_parse_get "ingresses"
}

__kubectl_get_limits() {
  __kubectl_parse_get "limitranges"
}

__kubectl_get_ns() {
  __kubectl_parse_get "namespaces"
}

__kubectl_get_netpol() {
  __kubectl_parse_get "networkpolicies"
}

__kubectl_get_node() {
  __kubectl_parse_get "nodes"
}

__kubectl_get_pod() {
  __kubectl_parse_get "pod"
}

__kubectl_get_pvc() {
  __kubectl_parse_get "persistentvolumeclaims"
}

__kubectl_get_pv() {
  __kubectl_parse_get "persistentvolumes"
}

__kubectl_get_pdb() {
  __kubectl_parse_get "poddisruptionbudgets"
}

__kubectl_get_po() {
  __kubectl_parse_get "pods"
}

__kubectl_get_psp() {
  __kubectl_parse_get "podsecuritypolicies"
}

__kubectl_get_rs() {
  __kubectl_parse_get "replicasets"
}

__kubectl_get_rc() {
  __kubectl_parse_get "replicationcontrollers"
}

__kubectl_get_quota() {
  __kubectl_parse_get "resourcequotas"
}

__kubectl_get_secrets() {
  __kubectl_parse_get "secrets"
}

__kubectl_get_sa() {
  __kubectl_parse_get "serviceaccounts"
}

__kubectl_get_svc() {
  __kubectl_parse_get "services"
}

__kubectl_get_ss() {
  __kubectl_parse_get "statefulsets"
}

__kubectl_get_sc() {
  __kubectl_parse_get "storageclasses"
}



























__kubectl_config_users() {
  __kubectl_parse_config "users"
}

__kubectl_config_contexts() {
  __kubectl_parse_config "contexts"
}

__kubectl_config_clusters() {
  __kubectl_parse_config "clusters"
}

_completion_loader kubectl

# This command is used ALOT both below and in daily life
function k() {
  kubectl "$@"
}
__kubectl_complete k _kubectl

## Apply

function ka() {
  k apply "$@"
}
__kubectl_complete ka _kubectl_apply

## Attach

function katt() {
  k attach "$@"
}
__kubectl_complete katt _kubectl_attach

## Cluster Info

### Get a list of services started on a cluster by kube system;
###
### Get the public IP address of one of your nodes. Either, `kubectl
### cluster-info`, or if you are using Google Compute Engine instances, you
### can use the `gcloud compute instances list` command to see the public
### addresses of your nodes.

function kci() {
  k cluster-info "$@"
}
__kubectl_complete kci _kubectl_cluster-info

## Config

function kc() {
  k config "$@"
}
__kubectl_complete kc _kubectl_config

### Context

function kccc() {
  kc current-context "$@"
}
__kubectl_complete kccc _kubectl_config_current-context

#### Manage configuration quickly to switch contexts between local, dev ad staging.

function kcuc() {
  kc use-context "$@"
}
__kubectl_complete kcuc _kubectl_config_use-context __kubectl_config_contexts

function kcgc() {
  kc get-contexts "$@"
}
__kubectl_complete kcgc _kubectl_config_get-contexts

function kcsc() {
  kc set-context "$@"
}
__kubectl_complete kcsc _kubectl_config_set-context __kubectl_config_contexts

#### Setting the namespace preference

function kcscns() {
  if [ $# -eq 0 ]; then
    kcsc "$(kccc)" --namespace='default'
  else
    kcsc "$(kccc)" --namespace="$@"
  fi
}
__kubectl_complete kcscns _kubectl_config_set-context __kubectl_get_ns

#### get namespace of context, defaulting to current-context
function kcgcns() {
  local context
  local namespace

  if [ $# -eq 0 ]; then
    context="$(kccc)"
  else
    context="$1"
  fi

  namespace=$(kcgc "${context}" | tail -n +2 | sed 's/^\*/ /' | awk '{ print $4}' | sed -e 's/^[ \t]*//')
  { [[ -z $namespace ]] && echo "default"; } || echo "$namespace"
}
__kubectl_complete kcgcns _kubectl_config_get-contexts __kubectl_config_contexts


function kcdc() {
  kc delete-context "$@"
}
__kubectl_complete kcdc _kubectl_config_delete-context __kubectl_config_contexts

function kcrc() {
  kc rename-context "$@"
}
__kubectl_complete kcrc _kubectl_config_rename-context __kubectl_config_contexts

### Cluster

function kcgcl() {
  kc get-clusters "$@"
}
__kubectl_complete kcgcl _kubectl_config __kubectl_config_clusters

function kcscl() {
  kc set-cluster "$@"
}
__kubectl_complete kcscl _kubectl_config_set-cluster __kubectl_config_clusters

function kcdcl() {
  kc delete-cluster "$@"
}
__kubectl_complete kcdcl _kubectl_config_delete-cluster __kubectl_config_clusters

### Credentials

function kcscr() {
  kc set-credentials "$@"
}
__kubectl_complete kcscr _kubectl_config_set-credentials

### View

function kcv() {
  kc view "$@"
}
__kubectl_complete kcv _kubectl_config_view

function kcs() {
  kc set "$@"
}
__kubectl_complete kcs _kubectl_config_set

function kcu() {
  kc unset "$@"
}
__kubectl_complete kcu _kubectl_config_unset

function kcls() {
  # list clusters
  local cluster_names=$(kcgcl | tail -n +2 | sed 's/^\*/ /' | awk '{ print $1}')
  local context_names=$(kcgc | tail -n +2 | sed 's/^\*/ /' | awk '{ print $1, $2}')
  local entries=( $(kcgc | tail -n +2 | sed 's/^\*/ /' | awk '{ print $1","$2}') )
  declare -A clusters
  for entry in "${entries[@]}"; do
    local entryname=$(echo "$entry" | awk -F, '{print $1}');
    local entrycluster=$(echo "$entry" | awk -F, '{print $2}');

    if [ -z ${clusters[count_${entrycluster}]} ]; then
      clusters[count_${entrycluster}]=0;
    fi;

    local entrycount=${clusters[count_${entrycluster}]};
    ((++entrycount));
    clusters[count_${entrycluster}]=$entrycount;

    clusters["${entrycluster}|${entrycount}"]=$entryname;
  done

  if [ ${#cluster_names[@]} -eq 0 ]; then
    echo "No clusters"
  else
    local j=0
    for name in ${cluster_names[@]}; do
      if [ $j -gt 0 ]; then
        echo ""
      fi

      echo "Cluster: $name"
      echo "Contexts"

      context_count=${clusters[count_${name}]}
      for i in $(seq 1 "${context_count}"); do
        echo "  - ${clusters[${name}|$i]}";
      done
      unset context_count

      ((++j))
    done
  fi
}

## Copy

function kcp() {
  k cp "$@"
}
__kubectl_complete kcp _kubectl_cp

## Create

function kcr() {
  k create "$@"
}
__kubectl_complete kcr _kubectl_create

## Delete

function krm() {
  k delete "$@"
}
__kubectl_complete krm _kubectl_delete

function krmcsr() {
  krm certificatesigningrequests "$@"
}
__kubectl_complete krmcsr _kubectl_delete __kubectl_get_csr

function krmcs() {
  krm componentstatuses "$@"
}
__kubectl_complete krmcs _kubectl_delete __kubectl_get_cs

function krmcm() {
  krm configmaps "$@"
}
__kubectl_complete krmcm _kubectl_delete __kubectl_get_cm

function krmcrd() {
  krm customresourcedefinition "$@"
}
__kubectl_complete krmcrd _kubectl_delete __kubectl_get_crd

function krmds() {
  krm daemonsets "$@"
}
__kubectl_complete krmds _kubectl_delete __kubectl_get_ds

function krmd() {
  krm deployments "$@"
}
__kubectl_complete krmd _kubectl_delete __kubectl_get_deploy

function krmdeploy() {
  krm deployments "$@"
}
__kubectl_complete krmdeploy _kubectl_delete __kubectl_get_deploy

function krmep() {
  krm endpoints "$@"
}
__kubectl_complete krmep _kubectl_delete __kubectl_get_ep

function krmev() {
  krm events "$@"
}
__kubectl_complete krmev _kubectl_delete __kubectl_get_ev

function krmhpa() {
  krm horizontalpodautoscalers "$@"
}
__kubectl_complete krmhpa _kubectl_delete __kubectl_get_hpa

function krming() {
  krm ingresses "$@"
}
__kubectl_complete krming _kubectl_delete __kubectl_get_ing

function krmlimits() {
  krm limitranges "$@"
}
__kubectl_complete krmlimits _kubectl_delete __kubectl_get_limits

function krmns() {
  krm namespaces "$@"
}
__kubectl_complete krmns _kubectl_delete __kubectl_get_ns

function krmnetpol() {
  krm networkpolicies "$@"
}
__kubectl_complete krmnetpol _kubectl_delete __kubectl_get_netpol

function krmno() {
  krm nodes "$@"
}
__kubectl_complete krmno _kubectl_delete __kubectl_get_node

function krmpvc() {
  krm persistentvolumeclaims "$@"
}
__kubectl_complete krmpvc _kubectl_delete __kubectl_get_pvc

function krmpv() {
  krm persistentvolumes "$@"
}
__kubectl_complete krmpv _kubectl_delete __kubectl_get_pv

function krmpdb() {
  krm poddisruptionbudgets "$@"
}
__kubectl_complete krmpdb _kubectl_delete __kubectl_get_pdb

function krmp() {
  krm pods "$@"
}
__kubectl_complete krmpo _kubectl_delete __kubectl_get_pod

function krmpsp() {
  krm podsecuritypolicies "$@"
}
__kubectl_complete krmpsp _kubectl_delete __kubectl_get_psp

function krmrs() {
  krm replicasets "$@"
}
__kubectl_complete krmrs _kubectl_delete __kubectl_get_rs

function krmrc() {
  krm replicationcontrollers "$@"
}
__kubectl_complete krmrc _kubectl_delete __kubectl_get_rc

function krmquota() {
  krm resourcequotas "$@"
}
__kubectl_complete krmquota _kubectl_delete __kubectl_get_quota

function krms() {
  krm secrets "$@"
}
__kubectl_complete krms _kubectl_delete __kubectl_get_secrets

function krmsa() {
  krm serviceaccounts "$@"
}
__kubectl_complete krmsa _kubectl_delete __kubectl_get_sa

function krmsvc() {
  krm services "$@"
}
__kubectl_complete krmsvc _kubectl_delete __kubectl_get_svc

function krmss() {
  krm statefulsets "$@"
}
__kubectl_complete krmss _kubectl_delete __kubectl_get_ss

function krmsc() {
  krm storageclasses "$@"
}
__kubectl_complete krmsc _kubectl_delete __kubectl_get_sc

## Describe

function kd() {
  k describe "$@"
}
__kubectl_complete kd _kubectl_describe

function kdcsr() {
  kd certificatesigningrequests "$@"
}
__kubectl_complete kdcsr _kubectl_describe __kubectl_get_csr

function kdcs() {
  kd componentstatuses "$@"
}
__kubectl_complete kdcs _kubectl_describe __kubectl_get_cs

function kdcm() {
  kd configmaps "$@"
}
__kubectl_complete kdcm _kubectl_describe __kubectl_get_cm

function kdcrd() {
  kd customresourcedefinition "$@"
}
__kubectl_complete kdcrd _kubectl_describe __kubectl_get_crd

function kdds() {
  kd daemonsets "$@"
}
__kubectl_complete kdds _kubectl_describe __kubectl_get_ds

function kdd() {
  kd deployments "$@"
}
__kubectl_complete kdd _kubectl_describe __kubectl_get_deploy

function kddeploy() {
  kd deployments "$@"
}
__kubectl_complete kddeploy _kubectl_describe __kubectl_get_deploy

function kdep() {
  kd endpoints "$@"
}
__kubectl_complete kdep _kubectl_describe __kubectl_get_ep

function kdev() {
  kd events "$@"
}
__kubectl_complete kdev _kubectl_describe __kubectl_get_ev

function kdhpa() {
  kd horizontalpodautoscalers "$@"
}
__kubectl_complete kdhpa _kubectl_describe __kubectl_get_hpa

function kding() {
  kd ingresses "$@"
}
__kubectl_complete kding _kubectl_describe __kubectl_get_ing

function kdlimits() {
  kd limitranges "$@"
}
__kubectl_complete kdlimits _kubectl_describe __kubectl_get_limits

function kdns() {
  kd namespaces "$@"
}
__kubectl_complete kdns _kubectl_describe __kubectl_get_ns

function kdnetpol() {
  kd networkpolicies "$@"
}
__kubectl_complete kdnetpol _kubectl_describe __kubectl_get_netpol

function kdno() {
  kd nodes "$@"
}
__kubectl_complete kdno _kubectl_describe __kubectl_get_node

function kdpvc() {
  kd persistentvolumeclaims "$@"
}
__kubectl_complete kdpvc _kubectl_describe __kubectl_get_pvc

function kdpv() {
  kd persistentvolumes "$@"
}
__kubectl_complete kdpv _kubectl_describe __kubectl_get_pv

function kdpdb() {
  kd poddisruptionbudgets "$@"
}
__kubectl_complete kdpdb _kubectl_describe __kubectl_get_pdb

function kdp() {
  kd pods "$@"
}
__kubectl_complete kdpo _kubectl_describe __kubectl_get_pod

function kdpsp() {
  kd podsecuritypolicies "$@"
}
__kubectl_complete kdpsp _kubectl_describe __kubectl_get_psp

function kdrs() {
  kd replicasets "$@"
}
__kubectl_complete kdrs _kubectl_describe __kubectl_get_rs

function kdrc() {
  kd replicationcontrollers "$@"
}
__kubectl_complete kdrc _kubectl_describe __kubectl_get_rc

function kdquota() {
  kd resourcequotas "$@"
}
__kubectl_complete kdquota _kubectl_describe __kubectl_get_quota

function kds() {
  kd secrets "$@"
}
__kubectl_complete kds _kubectl_describe __kubectl_get_secrets

function kdsa() {
  kd serviceaccounts "$@"
}
__kubectl_complete kdsa _kubectl_describe __kubectl_get_sa

function kdsvc() {
  kd services "$@"
}
__kubectl_complete kdsvc _kubectl_describe __kubectl_get_svc

function kdss() {
  kd statefulsets "$@"
}
__kubectl_complete kdss _kubectl_describe __kubectl_get_ss

function kdsc() {
  kd storageclasses "$@"
}
__kubectl_complete kdsc _kubectl_describe __kubectl_get_sc

## Edit

function ked() {
  k edit "$@"
}
__kubectl_complete ked _kubectl_edit

function kedcsr() {
  ked certificatesigningrequests "$@"
}
__kubectl_complete kedcsr _kubectl_edit __kubectl_get_csr

function kedcs() {
  ked componentstatuses "$@"
}
__kubectl_complete kedcs _kubectl_edit __kubectl_get_cs

function kedcm() {
  ked configmaps "$@"
}
__kubectl_complete kedcm _kubectl_edit __kubectl_get_cm

function kedcrd() {
  ked customresourcedefinition "$@"
}
__kubectl_complete kedcrd _kubectl_edit __kubectl_get_crd

function kedds() {
  ked daemonsets "$@"
}
__kubectl_complete kedds _kubectl_edit __kubectl_get_ds

function kedd() {
  ked deployments "$@"
}
__kubectl_complete kedd _kubectl_edit __kubectl_get_deploy

function keddeploy() {
  ked deployments "$@"
}
__kubectl_complete keddeploy _kubectl_edit __kubectl_get_deploy

function kedep() {
  ked endpoints "$@"
}
__kubectl_complete kedep _kubectl_edit __kubectl_get_ep

function kedev() {
  ked events "$@"
}
__kubectl_complete kedev _kubectl_edit __kubectl_get_ev

function kedhpa() {
  ked horizontalpodautoscalers "$@"
}
__kubectl_complete kedhpa _kubectl_edit __kubectl_get_hpa

function keding() {
  ked ingresses "$@"
}
__kubectl_complete keding _kubectl_edit __kubectl_get_ing

function kedlimits() {
  ked limitranges "$@"
}
__kubectl_complete kedlimits _kubectl_edit __kubectl_get_limits

function kedns() {
  ked namespaces "$@"
}
__kubectl_complete kedns _kubectl_edit __kubectl_get_ns

function kednetpol() {
  ked networkpolicies "$@"
}
__kubectl_complete kednetpol _kubectl_edit __kubectl_get_netpol

function kedno() {
  ked nodes "$@"
}
__kubectl_complete kedno _kubectl_edit __kubectl_get_node

function kedpvc() {
  ked persistentvolumeclaims "$@"
}
__kubectl_complete kedpvc _kubectl_edit __kubectl_get_pvc

function kedpv() {
  ked persistentvolumes "$@"
}
__kubectl_complete kedpv _kubectl_edit __kubectl_get_pv

function kedpdb() {
  ked poddisruptionbudgets "$@"
}
__kubectl_complete kedpdb _kubectl_edit __kubectl_get_pdb

function kedp() {
  ked pods "$@"
}
__kubectl_complete kedpo _kubectl_edit __kubectl_get_pod

function kedpsp() {
  ked podsecuritypolicies "$@"
}
__kubectl_complete kedpsp _kubectl_edit __kubectl_get_psp

function kedrs() {
  ked replicasets "$@"
}
__kubectl_complete kedrs _kubectl_edit __kubectl_get_rs

function kedrc() {
  ked replicationcontrollers "$@"
}
__kubectl_complete kedrc _kubectl_edit __kubectl_get_rc

function kedquota() {
  ked resourcequotas "$@"
}
__kubectl_complete kedquota _kubectl_edit __kubectl_get_quota

function keds() {
  ked secrets "$@"
}
__kubectl_complete keds _kubectl_edit __kubectl_get_secrets

function kedsa() {
  ked serviceaccounts "$@"
}
__kubectl_complete kedsa _kubectl_edit __kubectl_get_sa

function kedsvc() {
  ked services "$@"
}
__kubectl_complete kedsvc _kubectl_edit __kubectl_get_svc

function kedss() {
  ked statefulsets "$@"
}
__kubectl_complete kedss _kubectl_edit __kubectl_get_ss

function kedsc() {
  ked storageclasses "$@"
}
__kubectl_complete kedsc _kubectl_edit __kubectl_get_sc

## Exec

function kex() {
  k exec "$@"
}
__kubectl_complete kex _kubectl_exec

function kx() {
  k exec "$@"
}
__kubectl_complete kx _kubectl_exec

### Drop into an interactive terminal on a container
function kexti() {
  kex -ti "$@"
}
__kubectl_complete kexti _kubectl_exec

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

function kxandrew() {
  if k get pod --selector run=andrew 2>/dev/null | grep andrew > /dev/null 2>&1; then
    kxbash andrew
  else
    k run -ti andrew --image=reg.iherb.net/tools/docker-andrew:latest --generator="run-pod/v1" "$@";
  fi
}

## Explain

function kep() {
  k explain "$@"
}
__kubectl_complete kex _kubectl_explain

function kepcsr() {
  kep certificatesigningrequests "$@"
}
__kubectl_complete kepcsr _kubectl_explain

function kepcs() {
  kep componentstatuses "$@"
}
__kubectl_complete kepcs _kubectl_explain

function kepcm() {
  kep configmaps "$@"
}
__kubectl_complete kepcm _kubectl_explain

function kepcrd() {
  kep customresourcedefinition "$@"
}
__kubectl_complete kepcrd _kubectl_explain

function kepds() {
  kep daemonsets "$@"
}
__kubectl_complete kepds _kubectl_explain

function kepd() {
  kep deployments "$@"
}
__kubectl_complete kepd _kubectl_explain

function kepdeploy() {
  kep deployments "$@"
}
__kubectl_complete kepdeploy _kubectl_explain

function kepep() {
  kep endpoints "$@"
}
__kubectl_complete kepep _kubectl_explain

function kepev() {
  kep events "$@"
}
__kubectl_complete kepev _kubectl_explain

function kephpa() {
  kep horizontalpodautoscalers "$@"
}
__kubectl_complete kephpa _kubectl_explain

function keping() {
  kep ingresses "$@"
}
__kubectl_complete keping _kubectl_explain

function keplimits() {
  kep limitranges "$@"
}
__kubectl_complete keplimits _kubectl_explain

function kepns() {
  kep namespaces "$@"
}
__kubectl_complete kepns _kubectl_explain

function kepnetpol() {
  kep networkpolicies "$@"
}
__kubectl_complete kepnetpol _kubectl_explain

function kepno() {
  kep nodes "$@"
}
__kubectl_complete kepno _kubectl_explain

function keppvc() {
  kep persistentvolumeclaims "$@"
}
__kubectl_complete keppvc _kubectl_explain

function keppv() {
  kep persistentvolumes "$@"
}
__kubectl_complete keppv _kubectl_explain

function keppdb() {
  kep poddisruptionbudgets "$@"
}
__kubectl_complete keppdb _kubectl_explain

function kepp() {
  kep pods "$@"
}
__kubectl_complete keppo _kubectl_explain

function keppsp() {
  kep podsecuritypolicies "$@"
}
__kubectl_complete keppsp _kubectl_explain

function keprs() {
  kep replicasets "$@"
}
__kubectl_complete keprs _kubectl_explain

function keprc() {
  kep replicationcontrollers "$@"
}
__kubectl_complete keprc _kubectl_explain

function kepquota() {
  kep resourcequotas "$@"
}
__kubectl_complete kepquota _kubectl_explain

function keps() {
  kep secrets "$@"
}
__kubectl_complete keps _kubectl_explain

function kepsa() {
  kep serviceaccounts "$@"
}
__kubectl_complete kepsa _kubectl_explain

function kepsvc() {
  kep services "$@"
}
__kubectl_complete kepsvc _kubectl_explain

function kepss() {
  kep statefulsets "$@"
}
__kubectl_complete kepss _kubectl_explain

function kepsc() {
  kep storageclasses "$@"
}
__kubectl_complete kepsc _kubectl_explain

## Expose

function keo() {
  k expose "$@"
}
__kubectl_complete kex _kubectl_expose

## Get

function kg() {
  k get -o wide "$@"
}
__kubectl_complete kg _kubectl_get

function kgj() {
  k get -o json "$@" | jq
}
__kubectl_complete kgj _kubectl_get

function kgy() {
  k get -o yaml "$@"
}
__kubectl_complete kgy _kubectl_get

#### get namespace of context, defaulting to current-context
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

kls() {
  local namespaces=( $(kgns | tail -n +2 | awk '{ print $1 }') )
  local current_namespace=$(kcgcns)

  local j=0
  for namespace in "${namespaces[@]}"; do
    if [ $j -gt 0 ]; then
      echo ""
    fi

    local result
    if [ $namespace = $current_namespace ]; then
       echo "## Namespace: ${namespace} *"
    else
       echo "## Namespace: $namespace"
    fi

    if [ $# -eq 0 ]; then
      for cmd in pods deployments daemonsets statefulsets replicasets replicationcontrollers jobs ingress services endpoints persistentvolumeclaims persistentvolumes storageclasses secrets configmaps serviceaccounts; do
        result=$(k get --namespace=$namespace $cmd 2>&1)
        if ! echo "$result" | grep 'No resources found' > /dev/null 2>&1; then
          echo ""
          echo "### $cmd"
          echo ""
          echo "$result"
        fi
      done
    else
      result=$(k get --namespace=$namespace "$@" 2>&1)
      if ! echo "$result" | grep 'No resources found' > /dev/null 2>&1; then
        echo ""
        echo "$result"
      fi
    fi

    ((++j))
  done
}
__kubectl_complete kls _kubectl_get __kubectl_get_resource

kll() {
  local namespaces=( $(kgns | tail -n +2 | awk '{ print $1 }') )
  local current_namespace=$(kcgcns)

  local j=0
  for namespace in "${namespaces[@]}"; do
    if [ $j -gt 0 ]; then
      echo ""
    fi

    local result
    if [ $namespace = $current_namespace ]; then
       echo "## Namespace: ${namespace} *"
    else
       echo "## Namespace: $namespace"
    fi

    if [ $# -eq 0 ]; then
      for cmd in pods deployments daemonsets statefulsets replicasets replicationcontrollers jobs ingress services endpoints persistentvolumeclaims persistentvolumes storageclasses secrets configmaps serviceaccounts; do
        result=$(kg --namespace=$namespace $cmd 2>&1)
        if ! echo "$result" | grep 'No resources found' > /dev/null 2>&1; then
          echo ""
          echo "### $cmd"
          echo ""
          echo "$result"
        fi
      done
    else
      result=$(kg --namespace=$namespace "$@" 2>&1)
      if ! echo "$result" | grep 'No resources found' > /dev/null 2>&1; then
        echo ""
        echo "$result"
      fi
    fi

    ((++j))
  done

}
__kubectl_complete kll _kubectl_get __kubectl_get_resource

## Logs

function kl() {
  k logs "$@"
}
__kubectl_complete kl _kubectl_logs __kubectl_get_pod

function klf() {
  kl --tail=200 -f "$@"
}
__kubectl_complete klf _kubectl_logs __kubectl_get_pod

function klogs() {
  k logs "$@"
}
__kubectl_complete klogs _kubectl_logs __kubectl_get_pod

## Options

function ko() {
  k options "$@"
}
__kubectl_complete ko _kubectl_options

## Run

function krun() {
  k run "$@"
}
__kubectl_complete ko _kubectl_run

## Replace

function krep() {
  k replace "$@"
}
__kubectl_complete krep _kubectl_replace

## Rollout

function kro() {
  k rollout "$@"
}
__kubectl_complete kro _kubectl_rollout

### Rollout management.

function kroh() {
  kro history "$@"
}
__kubectl_complete kroh _kubectl_rollout_history

function krop() {
  kro pause "$@"
}
__kubectl_complete krop _kubectl_rollout_pause

function kror() {
  kro resume "$@"
}
__kubectl_complete kror _kubectl_rollout_resume

function kros() {
  kro status "$@"
}
__kubectl_complete kros _kubectl_rollout_status

function krou() {
  kro undo "$@"
}
__kubectl_complete krou _kubectl_rollout_undo

## Scale

function ksc() {
  k scale "$@"
}
__kubectl_complete ksc _kubectl_scale

## Set

function ks() {
  k set "$@"
}
__kubectl_complete ks _kubectl_set

## Version

function kv() {
  k version "$@"
}
__kubectl_complete kv _kubectl_version

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
