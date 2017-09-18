if cask_contains_element "minikube" || \
    hash minikube 2>/dev/null; then

    export MINIKUBE_WANTUPDATENOTIFICATION=false
    export MINIKUBE_WANTREPORTERRORPROMPT=false
    export MINIKUBE_HOME=$HOME
    export CHANGE_MINIKUBE_NONE_USER=true
    # mkdir $HOME/.kube || true
    # touch $HOME/.kube/config

    export KUBECONFIG=$HOME/.kube/config
    # sudo -E ./minikube start --vm-driver=none

    # this for loop waits until kubectl can access the api server that minikube has created
    # for i in {1..150} # timeout for 5 minutes
    # do
    #     ./kubectl get po &> /dev/null
    #     if [ $? -ne 1 ]; then
    #         break
    #     fi
    #     sleep 2
    # done

    # kubectl commands are now able to interact with minikube cluster
fi
