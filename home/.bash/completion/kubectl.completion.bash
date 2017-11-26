if brew_contains_element "kubectl" || \
    hash kubectl 2>/dev/null; then

  kubectl() {
    echo "loading kubectl completion..."
    unset kubectl

    kubectl_which=${BREW_HOME}/bin/kubectl
    if [ ! -f ${kubectl_which} ]; then
      kubectl_which=$(which kubectl)
    fi

    source <(${kubectl_which} completion bash)

    if [ -e "${BASH_IT}/aliases/kubectl.aliases.delayed.bash" ]; then
      . "${BASH_IT}/aliases/kubectl.aliases.delayed.bash"
    fi

    if [ $# -eq 0 ]; then
      ${kubectl_which} >/dev/null
    else
      ${kubectl_which} "$@"
    fi
    unset kubectl_which
  }

  k() {
    unset k
    kubectl
  }
fi
