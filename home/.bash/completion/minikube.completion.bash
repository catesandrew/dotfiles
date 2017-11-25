if cask_contains_element "minikube" || \
    hash minikube 2>/dev/null; then

    minikube() {
      echo "loading minikube completion..."
      if [ -f ${BREW_HOME}/bin/minikube ]; then
        source <(${BREW_HOME}/bin/minikube completion bash)
        unset minikube
        ${BREW_HOME}/bin/minikube "$@"
      else
        minikube_which=$(which minikube)
        source <(${minikube_which} completion bash)
        unset minikube
        ${minikube_which} "$@"
      fi
    }
fi
