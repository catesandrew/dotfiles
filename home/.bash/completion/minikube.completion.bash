if cask_contains_element "minikube" || \
    hash minikube 2>/dev/null; then

  minikube() {
    echo "loading minikube completion..."
    unset minikube

    minikube_which=${BREW_HOME}/bin/minikube
    if [ ! -f ${minikube_which} ]; then
      minikube_which=$(which minikube)
    fi

    source <(${minikube_which} completion bash)

    if [ -e "${BASH_IT}/aliases/minikube.aliases.delayed.bash" ]; then
      . "${BASH_IT}/aliases/minikube.aliases.delayed.bash"
    fi

    ${minikube_which} "$@"
    unset minikube_which
  }

  mk() {
    unset mk
    minikube
  }
fi
