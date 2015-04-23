# http://brettterpstra.com/2015/04/01/intrepid-command-line-directory-traversal/

# Here’s one of my own functions for jumping to subdirectories up to three
# levels from the working directory using fuzzy search. It incorporates fzf,
# which is brilliant and you should have it anyway.
#
# With this function you can drill into a directory tree quickly. If you’re in
# ~/Code/marked and you want to get to ~/Code/marked/vendor/multimarkdown/, you
# can just type cdd mmd and it will find the target and jump to it. If there
# are multiple matches, you’ll get the fzf screen where you can use arrow keys
# or typeahead filtering to pick the correct target.

# choose cd dir from menu (fzf)
# fuzzy search 3 levels deep
cdd() {
	local needle=$(echo "$*" | sed -E 's/ +/.*/g')
	cd "`find . -type d -maxdepth 3 | grep -Ei "${needle}[^/]*$" | fzf -s 20 -1 -0 -q "$1"`"
}

# You can replace `fzf` command on each example with
# [`fzf-tmux`](https://github.com/junegunn/fzf#fzf-tmux-script) to start fzf in
# a new tmux split pane.

### Opening files

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local file
  file=$(fzf --query="$1" --select-1 --exit-0)
  [ -n "$file" ] && ${EDITOR:-vim} "$file"
}

# Modified version where you can press
#   - CTRL-O to open with `open` command,
#   - CTRL-E or Enter key to open with the $EDITOR
fo() {
  local out file key
  out=$(fzf-tmux --query="$1" --exit-0 --expect=ctrl-o,ctrl-e)
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-vim} "$file"
  fi
}

### Changing directory

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# fda - including hidden directories
fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# Suggested by [@harelba](https://github.com/harelba) and
# [@dimonomid](https://github.com/dimonomid):

# fcd - cd into the directory of the selected file
fcd() {
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

### Searching file contents

# grep --line-buffered --color=never -r "" * | fzf

### Command history

# fh - repeat history
# fh() {
#   eval $(([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
# }
#
# fh - repeat history
# fh() {
#   print -z $(([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
# }

# Replacing `eval` with `print -z` will push the arguments onto the editing
# buffer stack, allowing you to edit the command before running it. It also
# means the command you run will appear in your history rather than just `fh`.
# Unfortunately this only works for zsh.

#### With write to terminal capabilities

# utility function used to write the command in the shell
# writecmd() {
#   perl -e '$TIOCSTI = 0x5412; $l = <STDIN>; $lc = $ARGV[0] eq "-run" ? "\n" : ""; $l =~ s/\s*$/$lc/; map { ioctl STDOUT, $TIOCSTI, $_; } split "", $l;' -- $1
# }
#
# # fh - repeat history
# fh() {
#   ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -re 's/^\s*[0-9]+\s*//' | writecmd -run
# }
#
# # fhe - repeat history edit
# fhe() {
#   ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -re 's/^\s*[0-9]+\s*//' | writecmd
# }


### Processes

# fkill - kill process
fkill() {
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}

### Git

# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch) &&
  branch=$(echo "$branches" | fzf +m) &&
  git checkout $(echo "$branch" | sed "s/.* //")
}

# fbr - checkout git branch (including remote branches)
fbr() {
  local branches branch
  branches=$(git branch --all | grep -v HEAD) &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

# fco - checkout git branch/tag
fco() {
  local tags branches target
  tags=$(
    git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
    git branch --all | grep -v HEAD             |
    sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
    sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
    (echo "$tags"; echo "$branches") |
    fzf-tmux -l30 -- --no-hscroll --ansi +m -d "\t" -n 2) || return
  git checkout $(echo "$target" | awk '{print $2}')
}

# fcoc - checkout git commit
fcoc() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

# fshow - git commit browser
fshow() {
  local out sha q
  while out=$(
      git log --decorate=short --graph --oneline --color=always |
      fzf --ansi --multi --no-sort --reverse --query="$q" --print-query); do
    q=$(head -1 <<< "$out")
    while read sha; do
      [ -n "$sha" ] && git show --color=always $sha | less -R
    done < <(sed '1d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
  done
}

### Tags

# ftags - search ctags
ftags() {
  local line
  [ -e tags ] &&
  line=$(
    awk 'BEGIN { FS="\t" } !/^!/ {print toupper($4)"\t"$1"\t"$2"\t"$3}' tags |
    cut -c1-80 | fzf --nth=1,2
  ) && $EDITOR $(cut -f3 <<< "$line") -c "set nocst" \
                                      -c "silent tag $(cut -f2 <<< "$line")"
}

### tmux

# fs [FUZZY PATTERN] - Select selected tmux session
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fs() {
  local session
  session=$(tmux list-sessions -F "#{session_name}" | \
    fzf --query="$1" --select-1 --exit-0) &&
  tmux switch-client -t "$session"
}

# ftpane - switch pane
ftpane () {
  local panes current_window target target_window target_pane
  panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
  current_window=$(tmux display-message  -p '#I')

  target=$(echo "$panes" | fzf) || return

  target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
  target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

  if [[ $current_window -eq $target_window ]]; then
    tmux select-pane -t ${target_window}.${target_pane}
  else
    tmux select-pane -t ${target_window}.${target_pane} &&
    tmux select-window -t $target_window
  fi
}

### v
# Inspired by [v](https://github.com/rupa/v). Opens files in ~/.viminfo

# v - open files in ~/.viminfo
# v() {
#   local files
#   files=$(grep '^>' ~/.viminfo | cut -c3- |
#           while read line; do
#             [ -f "${line/\~/$HOME}" ] && echo "$line"
#           done | fzf-tmux -d -m -q "$*" -1) && vim ${files//\~/$HOME}
# }

### z

# Integration with [z](https://github.com/rupa/z), like normal z when used with
# arguments but displays an fzf prompt when used without.

# j() {
#   if [[ -z "$*" ]]; then
#     cd "$(_j -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
#   else
#     _j "$@"
#   fi
# }

# Here is another version that also supports relaunching z with the arguments
# for the previous command as the default input by using zz

# Since z is not very optimal located on a qwerty keyboard I have these aliased
# as j and jj

j() {
  if [[ -z "$*" ]]; then
    cd "$(_j -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
  else
    _last_j_args="$@"
    _j "$@"
  fi
}

jj() {
  cd "$(_j -l 2>&1 | sed 's/^[0-9,.]* *//' | fzf -q $_last_j_args)"
}


### Google Chrome (OS X)

#### Browsing history

# c - browse chrome history
c() {
  local cols sep
  cols=$(( COLUMNS / 3 ))
  sep='{{::}}'

  # Copy History DB to circumvent the lock
  # - See http://stackoverflow.com/questions/8936878 for the file path
  cp -f ~/Library/Application\ Support/Google/Chrome/Profile\ 1/History /tmp/h

  sqlite3 -separator $sep /tmp/h \
    "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
  awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\n", $1, $2}' |
  fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs open
}

#### Bookmarks

# https://gist.github.com/junegunn/15859538658e449b886f (for OS X)

### Browsing

# fsfzf - browse file system
# https://github.com/D630/fzf-fs
# ```sh
# % . fsfzf.sh <ARG>
# ```

### Locate

# `Alt-i` to paste item from `locate /` output (zsh only):

# ```sh
# # ALT-I - Paste the selected entry from locate output into the command line
# fzf-locate-widget() {
#   local selected
#   if selected=$(locate / | fzf -q "$LBUFFER"); then
#     LBUFFER=$selected
#   fi  
#   zle redisplay
# }
# zle     -N    fzf-locate-widget
# bindkey '\ei' fzf-locate-widget
# ```

### RVM

# RVM integration
frb() {
  local rb
  rb=$((echo system; rvm list | grep ruby | cut -c 4-) |
       awk '{print $1}' |
       fzf-tmux -l 30 +m --reverse) && rvm use $rb
}
