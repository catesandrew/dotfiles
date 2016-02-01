#!/bin/bash

# TODO: rewrite these and put into .inputrc
# http://unix.stackexchange.com/questions/76566

# http://www.lugod.org/mailinglists/archives/vox-tech/2003-06/msg00182.html
# For those who want to use Vi bindings in bash, this corrects a
# few annoyances:
#
# 1) up and down arrows retrieve history lines even in insert mode
# 2) left and right arrows work in insert mode
# 3) Ctrl-A and Ctrl-E work how you expect if you have had to
#    live in Emacs mode in the past.
# 4) So does Ctrl-D.
# 5) And Ctrl-L.

## Command-mode bindings
# Ctrl-A or Home: insert at line beginning like in emacs mode
bind -m vi-command '\C-a: vi-insert-beg'
# Ctrl-E or End: append at line end like in emacs mode
bind -m vi-command '\C-e: vi-append-eol'
# to switch to emacs editing mode
bind -m vi-command '"ZZ": emacs-editing-mode'

## Insert-mode bindings
# up arrow or PgUp: append to previous history line
bind -m vi-insert '"\M-[A": ""'
bind -m vi-insert '"\M-[5~": ""'
# bind -m vi-insert '\C-p: previous-history'
# ^p check for partial match in history
bind -m vi-insert "\C-p":dynamic-complete-history
# dn arrow or PgDn: append to next history line
bind -m vi-insert '"\M-[B": ""'
bind -m vi-insert '"\M-[6~": ""'
# bind -m vi-insert '\C-n: next-history'
# ^n cycle through the list of partial matches
bind -m vi-insert "\C-n":menu-complete
# Ctrl-A: insert at line start like in emacs mode
bind -m vi-insert '\C-a: beginning-of-line'
# Ctrl-E: append at line end like in emacs mode
bind -m vi-insert '\C-e: end-of-line'
# Ctrl-D: delete character
bind -m vi-insert '\C-d: delete-char'
# Ctrl-L: clear screen
bind -m vi-insert "\C-l":clear-screen

## Specify vi editing mode
set -o vi

# I hate noise
set bell-style visible

## Emacs bindings
# Meta-V: go back to vi editing
bind -m emacs '"\ev": vi-editing-mode'

if [ -f "${HOME}/.bashrc" ]; then
  . "${HOME}/.bashrc"
fi
