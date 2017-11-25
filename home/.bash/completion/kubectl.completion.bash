if brew_contains_element "kubectl" || \
    hash kubectl 2>/dev/null; then

    kubectl() {
      echo "loading kubectl completion..."
      if [ -f ${BREW_HOME}/bin/kubectl ]; then
        source <(${BREW_HOME}/bin/kubectl completion bash)
        unset kubectl
        ${BREW_HOME}/bin/kubectl "$@"
      else
        kubectl_which=$(which kubectl)
        source <(${kubectl_which} completion bash)
        unset kubectl
        ${kubectl_which} "$@"
      fi
    }
fi
