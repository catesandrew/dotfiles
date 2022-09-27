#!/usr/local/bin/bash
# /usr/local/bin/emacsclient -n "$1"


# Number of current visible frames,
# Emacs daemon always has a visible frame called F1
em_visible_frames() {
  "/usr/local/bin/emacsclient" -e '(length (visible-frame-list))'
}

em_change_focus() {
  "/usr/local/bin/emacsclient" -n -e "(select-frame-set-input-focus (selected-frame))" > /dev/null
}
#
# shamelessly taken from http://mjwall.com/blog/2013/10/04/how-i-use-emacs/

# emacsclient options for reference
# -a "" starts emacs daemon and reattaches
# -c creates a new frame
# -n returns control back to the terminal
# -e eval the script

# try switching to the frame incase it is just minimized

test "$(em_visible_frames)" -eq "1" && em_change_focus

if [ "$(em_visible_frames)" -lt  "2" ]; then # need to create a frame
  # -c $@ with no args just opens the scratch buffer
  "/usr/local/bin/emacsclient" -n -c "$@" && em_change_focus
  "/usr/local/bin/emacsclient" -n -e "(set-frame-size (selected-frame) 150 48)" > /dev/null
else # there is already a visible frame besides the daemon, so
  em_change_focus
  # -n $@ errors if there are no args
  test  "$#" -ne "0" && "/usr/local/bin/emacsclient" -n "$@"
fi
