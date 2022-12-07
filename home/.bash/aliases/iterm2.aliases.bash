# load iterm2 aliases, if you are using it
if cask_contains_element "iterm2"; then
  if [[  "$-" == *i* && "$TERM" != linux && "$TERM" != dumb ]]; then
    alias imgcat=~/.iterm2/imgcat;
    alias imgls=~/.iterm2/imgls;
    alias it2api=~/.iterm2/it2api;
    alias it2attention=~/.iterm2/it2attention;
    alias it2check=~/.iterm2/it2check;
    alias it2copy=~/.iterm2/it2copy;
    alias it2dl=~/.iterm2/it2dl;
    alias it2getvar=~/.iterm2/it2getvar;
    alias it2git=~/.iterm2/it2git;
    alias it2setcolor=~/.iterm2/it2setcolor;
    alias it2setkeylabel=~/.iterm2/it2setkeylabel;
    alias it2tip=~/.iterm2/it2tip;
    alias it2ul=~/.iterm2/it2ul;
    alias it2universion=~/.iterm2/it2universion;
    alias it2profile=~/.iterm2/it2profile
  fi
fi
