if brew_contains_element "kubectl" || \
    hash kubectl 2>/dev/null; then

    source <(kubectl completion bash)
fi
