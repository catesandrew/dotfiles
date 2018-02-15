# load fzf, if you are using it

if brew_contains_element "fzf" && \
    [ -z "$INSIDE_EMACS" ]; then

    export FZF_TARGET="${BREW_HOME}/opt/fzf"
    export FZF_TMUX_HEIGHT=${FZF_TMUX_HEIGHT:-80%}

    # Auto-completion
    [[ $- == *i* ]] && source "${FZF_TARGET}/shell/completion.bash" 2> /dev/null

    # Key bindings
    . "${FZF_TARGET}/shell/key-bindings.bash"

    # Setting ag as the default source for fzf
    export FZF_DEFAULT_COMMAND='ag -l -g ""'

    # view what devices a running process has open `lsof -p 5051`
    complete -o bashdefault -o default -o nospace -F _fzf_complete_kill lsof
    # Viewing memory allocation with pmap (vmmap on osx). You can view the
    # memory allocations for a particular process with pmap (vmmap):
    complete -o bashdefault -o default -o nospace -F _fzf_complete_kill vmmap

elif hash fzf 2>/dev/null; then
    true
fi
