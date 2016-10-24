#!/bin/bash

# Notes:
#  - tsort requires as input a stream of pairs (a, b) where package a depends
#    on package b. If package a has k > 1 dependencies, we should have k lines
#    associated to it; if package a has no dependencies, then we should have a
#    single line (a, a). The pairs are just space delimited, no parentheses.
#    the little awk program below formats the data that way for tsort.
#  - tsort outputs the order from bottom to top; that's why we need to reverse
#    it with tail -r.

brew list \
  | while IFS= read -r l; do echo -n "$l "; echo "$(brew deps "$l")"; done \
  | awk 'NF == 1 {print $1, $1} NF > 1 {for (i=1;i<=NF;i++) print $1, $i}' \
  | tsort \
  | gtac \
  | while IFS= read -r l; do echo -n "$l "; brew info "$l" && brew reinstall "$l"; done
