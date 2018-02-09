# enter a few characters and press UpArrow/DownArrow
# to search backwards/forwards through the history
if [ -t 1 ] && \
     [ -z "$INSIDE_EMACS" ]; then

  # make option-up and option-down cycle through
  # commands that match the start of the line
  bind '"[A":history-search-backward'
  bind '"[B":history-search-forward'
fi
