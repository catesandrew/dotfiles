if cask_contains_element "minikube" || \
    hash minikube 2>/dev/null; then

    source <(minikube completion bash)
fi
