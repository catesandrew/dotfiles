# load fzf, if you are using it

if brew_contains_element "fzf" && \
  [ -z "$INSIDE_EMACS" ]; then

  export FZF_TARGET="${BREW_HOME}/opt/fzf"
  export FZF_TMUX_HEIGHT=${FZF_TMUX_HEIGHT:-80%}

  # Auto-completion
  [[ $- == *i* ]] && . "${FZF_TARGET}/shell/completion.bash" 2> /dev/null

  # Key bindings
  . "${FZF_TARGET}/shell/key-bindings.bash"

  # Setting ag as the default source for fzf
  export FZF_DEFAULT_COMMAND='rg --files --no-ignore --no-config --hidden --follow --glob "!.git/*"'
  export FZF_DEFAULT_OPTS='--reverse --border'
  export FZF_ALT_C_COMMAND="bfs -type d -nohidden | \sed s/^\./~/"
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

  _fzf_complete_kill() {
    [ -n "${COMP_WORDS[COMP_CWORD]}" ] && return 1
    local selected fzf
    fzf="$(__fzfcmd_complete)"
    selected=$(command psgrep -e | \sed 1d | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-50%} --min-height 15 --reverse $FZF_DEFAULT_OPTS --preview 'echo {}' --preview-window down:3:wrap $FZF_COMPLETION_OPTS" $fzf -m | awk '{print $2}' | tr '\n' ' ')
    printf '\e[5n'
    if [ -n "$selected" ]; then
      COMPREPLY=( "$selected" )
      return 0
    fi
  }

  # view what devices a running process has open `lsof -p 5051`
  complete -o bashdefault -o default -o nospace -F _fzf_complete_kill lsof
  # Viewing memory allocation with pmap (vmmap on osx). You can view the
  # memory allocations for a particular process with pmap (vmmap):
  complete -o bashdefault -o default -o nospace -F _fzf_complete_kill vmmap

elif hash fzf 2>/dev/null; then
    true
fi
