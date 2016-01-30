cite about-plugin
about-plugin 'load nvm, if you are using it'

if ! brew_contains_element "nvm"; then
    exit 0;
fi

# export NVM_TARGET="$(brew --prefix nvm)"
if [ -z "$NVM_TARGET" ]; then
    NVM_TARGET="$__dot_brew_home/opt/nvm"
    export NVM_TARGET
fi

if [ -z "$NVM_DIR" ]; then
    NVM_DIR="$__dot_brew_home/nvm"
    export NVM_DIR
fi

# lazy load NVM into a shell session *as a function*
nvm() {
    [ -s "$NVM_TARGET/nvm.sh" ] && . "$NVM_TARGET/nvm.sh"
    nvm "$@"
}
