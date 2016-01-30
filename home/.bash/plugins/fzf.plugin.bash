cite about-plugin
about-plugin 'load fzf, if you are using it'

if brew_contains_element "fzf"; then
    export FZF_TARGET="$__dot_brew_home/opt/fzf"

    # Auto-completion
    # [[ $- == *i* ]] && source "${FZF_TARGET}/shell/completion.bash" 2> /dev/null

    # lazy load fzf into a shell session *as a function*
    fzf() {
        . "${FZF_TARGET}/shell/completion.bash"
        fzf "$@"
    }

    fzf-tmux() {
        . "${FZF_TARGET}/shell/completion.bash"
        fzf-tmux "$@"
    }

    # Key bindings
    . "${FZF_TARGET}/shell/key-bindings.bash"

elif hash fzf 2>/dev/null; then
    true
fi

# Setting ag as the default source for fzf
export FZF_DEFAULT_COMMAND='ag -l -g ""'
