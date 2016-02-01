cite about-plugin
about-plugin 'load archey, if you are using it'

if ! brew_contains_element "archey"; then
    exit 0
fi

archey -o
