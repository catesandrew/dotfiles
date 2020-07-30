# load fzf, if you are using it

if brew_contains_element "fzf" && \
  [ -z "$INSIDE_EMACS" ]; then

  export FZF_TARGET="${BREW_HOME}/opt/fzf"
  export FZF_TMUX_HEIGHT=${FZF_TMUX_HEIGHT:-80%}

  # Auto-completion
  [[ $- == *i* ]] && source "${FZF_TARGET}/shell/completion.bash" 2> /dev/null

  # Key bindings
  source "${FZF_TARGET}/shell/key-bindings.bash"

  # Setting ag as the default source for fzf
  export FZF_DEFAULT_COMMAND='rg --files --no-ignore --no-config --hidden --follow --glob "!.git/*"'
  export FZF_DEFAULT_OPTS='--reverse --border'
  export FZF_ALT_C_COMMAND="bfs -type d -nohidden | \sed s/^\./~/"
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

  # This changes https://github.com/junegunn/fzf/commit/5cae8ea733e8663eae96480b5e6d4c94afcdb3bd#diff-1fabf11f4aca2d62eb64290f66d25217
  # investigate how to use fc with a different history file
  __fzf_history__() (
    local line
    shopt -u nocaseglob nocasematch
    line=$(
      { export HISTTIMEFORMAT= && export HISTFILE=${HOME}/.persistent_history && builtin history -cr $HISTFILE && history; } | tac | sort --key=2.1 -bus | sort -n |
      FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS --tac --sync -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m" $(__fzfcmd) |
      command grep '^ *[0-9]') &&
      if [[ $- =~ H ]]; then
        sed 's/^ *[0-9]*\** \(.*\)/\1/' <<< "$line"
      else
        sed 's/^ *[0-9]*\** \(*\)//' <<< "$line"
      fi
  )

  # CTRL-R - Paste the selected command from history into the command line
  bind -m emacs-standard '"\C-r": " \C-e\C-u\C-y\ey\C-u`__fzf_history__`\e\C-e\er\e^"'

  # CTRL-R - Paste the selected command from history into the command line
  bind -m vi-command '"\C-r": "\C-z\C-r\C-z"'
  bind -m vi-insert '"\C-r": "\C-z\C-r\C-z"'

  # view what devices a running process has open `lsof -p 5051`
  complete -o bashdefault -o default -o nospace -F _fzf_complete_kill lsof
  # Viewing memory allocation with pmap (vmmap on osx). You can view the
  # memory allocations for a particular process with pmap (vmmap):
  complete -o bashdefault -o default -o nospace -F _fzf_complete_kill vmmap

  if brew_contains_element "cheat" || \
      hash cheat 2>/dev/null; then
        # This function enables you to choose a cheatsheet to view by selecting output
        # from `cheat -l`. `source` it in your shell to enable it. (Consider renaming
        # or aliasing it to something convenient.)

        # Arguments passed to this function (like --color) will be passed to the second
        # invokation of `cheat`.
        function cheat-fzf {
          eval `cheat -l | tail -n +2 | fzf | awk -v vars="$*" '{ print "cheat " $1 " -t " $3, vars }'`
        }
  fi

elif hash fzf 2>/dev/null; then
    true
fi
