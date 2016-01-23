# TODO: Check for fzf not installed through brew
# Check first if homebrew/linuxbrew is installed, then check
# if fzf is installed before sourcing auto-completion
# and key-bindings for fzf
if hash brew 2>/dev/null; then
    if [ x"" != x"$(brew ls --versions fzf)" ]; then
        FZF_TARGET="$(brew --prefix fzf)"

        # Auto-completion
        # ---------------
        [[ $- == *i* ]] && source "${FZF_TARGET}/shell/completion.bash" 2> /dev/null

        # Key bindings
        # ------------
        source "${FZF_TARGET}/shell/key-bindings.bash"
    fi
fi
