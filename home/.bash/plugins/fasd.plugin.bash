# load fasd, if you are using it

# fasd_cache="$HOME/.fasd-init-bash"
# if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
#   fasd --init posix-alias posix-hook bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
# fi
# source "$fasd_cache"
# unset fasd_cache

if brew_contains_element "fasd" || \
    hash fasd 2>/dev/null; then

  _fasd_ps1_func() {
    { eval "fasd --proc $(fasd --sanitize $(fc -nl -1))"; } \
      >> "/dev/null" 2>&1
  }

  case $PS1 in
    *_fasd_ps1_func*) ;;
    *) export PS1="\$(_fasd_ps1_func)$PS1";;
  esac

  _fasd_prompt_func() {
    eval "fasd --proc $(fasd --sanitize $(history 1 | \
    sed "s/^[ ]*[0-9]*[ ]*//"))" >> "/dev/null" 2>&1
  }

  # add bash hook
  case $PROMPT_COMMAND in
    *_fasd_prompt_func*) ;;
    *) PROMPT_COMMAND="_fasd_prompt_func;$PROMPT_COMMAND";;
  esac

fi
