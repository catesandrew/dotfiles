# curl https://raw.githubusercontent.com/dcreemer/1pass/master/1pass > /usr/local/bin/1pass
# chmod a+x /usr/local/bin/1pass

if brew_contains_element "fzf" && \
    [ -z "$INSIDE_EMACS" ]; then

  _fzf_complete_pass ()
  {
    local arg selected password fzf;
    fzf="$(__fzfcmd_complete)";

    selected=$(1pass | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-50%} --min-height 15 --reverse $FZF_DEFAULT_OPTS --preview 'echo {}' --preview-window down:3:wrap $FZF_COMPLETION_OPTS" $fzf -m);
    printf '\e[5n';
    if [[ ! -z "$selected" ]]; then
      # password=$(1pass -p "${selected}" password)
      # echo "found password: $password"
      cmd=(-p);
      cmd+=("\"${selected}\"")
      cmd+=(password)
      COMPREPLY="${cmd[@]}"
      return 0;
    fi
  }
  complete -F _fzf_complete_pass -o default -o bashdefault 1pass

elif hash fzf 2>/dev/null; then
  true
fi
