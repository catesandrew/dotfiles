#!/bin/bash

# Theme Settings

SCM_GIT='git'
SCM_GIT_CHAR='±'

SCM_HG='hg'
SCM_HG_CHAR='☿'

SCM_SVN='svn'
SCM_SVN_CHAR='⑆'

SCM_NONE='NONE'
SCM_NONE_CHAR='○'

RVM_THEME_PROMPT_PREFIX=' |'
RVM_THEME_PROMPT_SUFFIX='|'

VIRTUALENV_THEME_PROMPT_PREFIX=' |'
VIRTUALENV_THEME_PROMPT_SUFFIX='|'

RBENV_THEME_PROMPT_PREFIX=' |'
RBENV_THEME_PROMPT_SUFFIX='|'

RBFU_THEME_PROMPT_PREFIX=' |'
RBFU_THEME_PROMPT_SUFFIX='|'

SCM_THEME_PROMPT_PREFIX=' |'
SCM_THEME_PROMPT_SUFFIX='|'

CLOCK_CHAR='☆'

SCM_DIRTY=0 # 3:staged, 2:unstaged, 1:dirty, 0:clean
THEME_PROMPT_HOST='\H'

export SCM_THEME_BRANCH_GONE_PREFIX=' ⇢ '
export SCM_THEME_BRANCH_TRACK_PREFIX=' → '
export SCM_THEME_DETACHED_PREFIX='⌿'
export SCM_THEME_PROMPT_CLEAN=' ✓'
export SCM_THEME_PROMPT_DIRTY=' ✗'
export SCM_THEME_TAG_PREFIX='tag:'

## GIT BASH PROMPT

export SCM_GIT_PROMPT_PREFIX="["           # start of the git info string
export SCM_GIT_PROMPT_SUFFIX="]"           # the end of the git info string
export SCM_GIT_PROMPT_SEPARATOR="|"        # separates each item
export SCM_GIT_PROMPT_STAGED="●"     # the number of staged files/directories
export SCM_GIT_PROMPT_CONFLICTS="✖ " # the number of files in conflict
export SCM_GIT_PROMPT_CHANGED="✚ "  # the number of changed files
export SCM_GIT_PROMPT_REMOTE=" " # the remote branch name (if any) and the symbols
                             # for ahead and behind
export SCM_GIT_PROMPT_UNTRACKED="…"     # the number of untracked files/dirs
export SCM_GIT_PROMPT_STASHED="⚑ " # the number of stashed files/dir
export SCM_GIT_PROMPT_CLEAN="✔" #${bold_green} a colored flag indicating a
                                 #"clean" repo

export SCM_GIT_PROMPT_DELETED="-" # the number of deleted files
export SCM_GIT_PROMPT_DELETED_CACHED="-" # the number of deleted files (staged)
export SCM_GIT_PROMPT_CHANGED_CACHED="✚"
export SCM_GIT_PROMPT_ADDS="+"

# For the command indicator, the placeholder _LAST_COMMAND_STATE_ will be
# replaced with the exit code of the last command e.g.
#
# indicator if the last command returned with an exit code of 0
# SCM_GIT_PROMPT_COMMAND_OK="$✔-_LAST_COMMAND_STATE_ " # {green}
# indicator if the last command returned with an exit code of other than 0
# SCM_GIT_PROMPT_COMMAND_FAIL="$✘-_LAST_COMMAND_STATE_ " # {red}

# indicator if the last command returned with an exit code of 0
export SCM_GIT_PROMPT_COMMAND_OK="✔" # ${green}
# indicator if the last command returned with an exit code of other than 0
export SCM_GIT_PROMPT_COMMAND_FAIL="✘-_LAST_COMMAND_STATE_" # ${red}

# template for displaying the current remote tracking branch use the placeholder
# _UPSTREAM_ will be replaced with the name of the current remote tracking
# branch
export SCM_GIT_PROMPT_UPSTREAM="_UPSTREAM_"

# _LAST_COMMAND_INDICATOR_ will be replaced by the appropriate GIT_PROMPT_COMMAND_OK OR GIT_PROMPT_COMMAND_FAIL

export SCM_GIT_PROMPT_START_USER="_LAST_COMMAND_INDICATOR_ ${yellow}\w${normal}"
export SCM_GIT_PROMPT_START_ROOT="${GIT_PROMPT_START_USER}"
export SCM_GIT_PROMPT_END_USER=" \n${white}${Time12a}${normal} $ "
export SCM_GIT_PROMPT_END_ROOT=" \n${white}${Time12a}${normal} # "

# Please do not add colors to these symbols

# The symbol for "n versions ahead of origin"
export SCM_GIT_PROMPT_SYMBOLS_AHEAD="↑·"
# The symbol for "n versions behind of origin"
export SCM_GIT_PROMPT_SYMBOLS_BEHIND="↓·"
# Written before hash of commit, if no name could be found
export SCM_GIT_PROMPT_SYMBOLS_PREHASH=${SCM_GIT_PROMPT_SYMBOLS_PREHASH:=:}
# This symbol is written after the branch, if the branch is not tracked
export SCM_GIT_PROMPT_SYMBOLS_NO_REMOTE_TRACKING=${SCM_GIT_PROMPT_SYMBOLS_NO_REMOTE_TRACKING:=L}
export SCM_GIT_PROMPT_SYMBOLS_TAG=${SCM_GIT_PROMPT_SYMBOLS_TAG:=T}

# This is the default theme for git bash prompt

Time12a="\$(date +%H:%M)"

# Switches for prompt

SCM_CHECK=${SCM_CHECK:=true}
GIT_SHOW_REMOTE_INFO=${GIT_SHOW_REMOTE_INFO:=auto}

# GIT_PROMPT_START=...    # uncomment for custom prompt start sequence
# GIT_PROMPT_END=...      # uncomment for custom prompt end sequence
GIT_PROMPT_FETCH_REMOTE_STATUS=1    # set to 1 to fetch remote status
GIT_PROMPT_SHOW_UPSTREAM=1          # set to 1 to show upstream tracking branch

# You can set the `GIT_PROMPT_SHOW_UNTRACKED_FILES` variable to `no` or `normal`
# to speed things up if you have lots of untracked files in your repository.
# This can be the case for build systems that put their build artifacts in the
# subdirectory structure of the git repository.
GIT_PROMPT_SHOW_UNTRACKED_FILES=all # can be `no`, `normal` or `all`; determines
                                    # counting of untracked files
GIT_PROMPT_FETCH_TIMEOUT="5" # fetch remote revisions every other (default 5) minutes
GIT_PROMPT_IGNORE_STASH=0

THEME_CLOCK_CHECK=${THEME_CLOCK_CHECK:=true}
THEME_BATTERY_PERCENTAGE_CHECK=${THEME_BATTERY_PERCENTAGE_CHECK:=true}

# Variables

export SCM_GIT_BRANCH
export SCM_GIT_REMOTE
export SCM_GIT_UPSTREAM
export SCM_GIT_STAGED
export SCM_GIT_CONFLICTS
export SCM_GIT_CHANGED
export SCM_GIT_UNTRACKED
export SCM_GIT_STASHED
export SCM_GIT_CLEAN
export SCM_GIT_ACTION
export SCM_GIT_UPSTREAM_SHORT
export SCM_GIT_HAS_DIVERGED
export SCM_GIT_HAS_UPSTREAM
export SCM_GIT_SHOULD_PUSH
export SCM_GIT_WILL_REBASE
export SCM_GIT_CHANGED_CACHED
export SCM_GIT_DELETED_CACHED
export SCM_GIT_DELETED
export SCM_GIT_ADDS
export SCM_GIT_AHEAD
export SCM_GIT_BEHIND
export SCM_GIT_GONE
export SCM_GIT_DETACHED
export SCM_GIT_ON_TAG
export SCM_GIT_TAG
export SCM_GIT_PREHASH

# Functions

function has_jshon() {
  if [[ -z "$_jshon_command" ]]; then
    if command -v jshon > /dev/null; then
      _jshon_command=jshon
    fi
  fi

  if [[ -n "$_jshon_command" ]]; then
    return 0
  fi

  return 1
}

# some versions of sed do not have -r
_have_sed_r=1

function string_length() {
  local matches
  local size
  local find_exit_code

  if [[ -z "$_sed_command" ]]; then
    if command -v gsed > /dev/null; then
      _sed_command=gsed
    else
      _sed_command=sed
    fi
  fi

  # http://www.commandlinefu.com/commands/view/3584/remove-color-codes-special-characters-with-sed
  if [[ "$_have_sed_r" = 1 ]]; then
    matches=$(echo "$1" | "$_sed_command" -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g" 2> /dev/null)
    find_exit_code="$?"
    if [[ -n "$matches" ]]; then
      size=${#matches}
      echo $size
      return 0
    else
      if [[ "$find_exit_code" != 0 ]]; then
        _have_sed_r=0
      else
        echo 0
        return 1
      fi
    fi
  fi

  return 1
}

# some versions of find do not have -mmin
_have_find_mmin=1

function older_than_minutes() {
  local matches
  local find_exit_code

  if [[ -z "$_find_command" ]]; then
    if command -v gfind > /dev/null; then
      _find_command=gfind
    else
      _find_command=find
    fi
  fi

  if [[ "$_have_find_mmin" = 1 ]]; then
    matches=$("$_find_command" "$1" -mmin +"$2" 2> /dev/null)
    find_exit_code="$?"
    if [[ -n "$matches" ]]; then
      return 0
    else
      if [[ "$find_exit_code" != 0 ]]; then
        _have_find_mmin=0
      else
        return 1
      fi
    fi
  fi

  GIT_PROMPT_FETCH_REMOTE_STATUS=0
  return 1
}

function prompt_callback_default {
  return
}

# Use exit status from declare command to determine whether input argument is a
# bash function
function is_function {
  declare -Ff "$1" >/dev/null;
}

# Git Functions

function replace_git_symbols() {
  local VALUE=${1//_AHEAD_/${SCM_GIT_PROMPT_SYMBOLS_AHEAD}}
  local VALUE1=${VALUE//_BEHIND_/${SCM_GIT_PROMPT_SYMBOLS_BEHIND}}
  local VALUE2=${VALUE1//_NO_REMOTE_TRACKING_/${SCM_GIT_PROMPT_SYMBOLS_NO_REMOTE_TRACKING}}
  local VALUE3=${VALUE2//_TAG_/${SCM_GIT_PROMPT_SYMBOLS_TAG}}

  echo "${VALUE3//_PREHASH_/${SCM_GIT_PROMPT_SYMBOLS_PREHASH}}"
}

function current_git_action () {
  local info
  local action

  info="$(git rev-parse --git-dir 2>/dev/null)"
  if [ -n "$info" ]; then
    if [ -f "$info/rebase-merge/interactive" ]; then
      action=${is_rebasing_interactively:-"rebase -i"}
    elif [ -d "$info/rebase-merge" ]; then
      action=${is_rebasing_merge:-"rebase -m"}
    else
      if [ -d "$info/rebase-apply" ]; then
        if [ -f "$info/rebase-apply/rebasing" ]; then
          action=${is_rebasing:-"rebase"}
        elif [ -f "$info/rebase-apply/applying" ]; then
          action=${is_applying_mailbox_patches:-"am"}
        else
          action=${is_rebasing_mailbox_patches:-"am/rebase"}
        fi
      elif [ -f "$info/MERGE_HEAD" ]; then
        action=${is_merging:-"merge"}
      elif [ -f "$info/CHERRY_PICK_HEAD" ]; then
        action=${is_cherry_picking:-"cherry-pick"}
      elif [ -f "$info/BISECT_LOG" ]; then
        action=${is_bisecting:-"bisect"}
      fi
    fi

    if [[ -n $action ]]; then
      echo -e "$action"
    fi
  fi
}

function check_git_upstream() {
  local repo
  repo="$(git rev-parse --show-toplevel 2> /dev/null)"
  local FETCH_HEAD="$repo/.git/FETCH_HEAD"
  # fetch repo if local is stale for more than $GIT_FETCH_TIMEOUT minutes
  if [[ ! -e "$FETCH_HEAD" ]] || older_than_minutes "$FETCH_HEAD" "$GIT_PROMPT_FETCH_TIMEOUT"; then
    if [[ -n $(git remote show) ]]; then
      (
        async_run "git fetch --quiet"
        disown -h
      )
    fi
  fi
}

function git_status() {
  local gitstatus
  gitstatus=$( LC_ALL=C git status --untracked-files=${GIT_PROMPT_SHOW_UNTRACKED_FILES:-all} --porcelain --branch )

  # if the status is fatal, exit now
  [[ "$?" -ne 0 ]] && exit 0

  local num_staged=0
  local num_changed=0
  local num_conflicts=0
  local num_untracked=0
  local num_delete=0
  local num_changed_cached=0
  local num_delete_cached=0
  local num_adds=0

  # X          Y     Meaning
  # -------------------------------------------------
  # [MD]   not updated
  # M        [ MD]   updated in index
  # A        [ MD]   added to index
  # D         [ M]   deleted from index
  # R        [ MD]   renamed in index
  # C        [ MD]   copied in index
  # [MARC]           index and work tree matches
  # [ MARC]     M    work tree changed since index
  # [ MARC]     D    deleted in work tree
  # -------------------------------------------------
  # D           D    unmerged, both deleted
  # A           U    unmerged, added by us
  # U           D    unmerged, deleted by them
  # U           A    unmerged, added by them
  # D           U    unmerged, deleted by us
  # A           A    unmerged, both added
  # U           U    unmerged, both modified
  # -------------------------------------------------
  # ?           ?    untracked
  # !           !    ignored
  # -------------------------------------------------

  while IFS='' read -r line || [[ -n "$line" ]]; do
    status=${line:0:2}
    case "$status" in
      # Replace `...` with a `^`
      # Before: ## master...origin/master
      # After: ## master^origin/master
      \#\#) branch_line="${line/\.\.\./^}" ;;
      \?\?) ((num_untracked++)) ;;
      [ARC\ ]M) ((num_changed++)) ;;
      M[\ ]) ((num_changed_cached++ & num_staged++)) ;;
      MM) ((num_changed_cached++ & num_staged++ & num_changed++)) ;;
      A[\ M]) ((num_adds++ & num_staged++)) ;;

      D[\ ]) ((num_delete_cached++ & num_staged++)) ;;
      [\ ARC]D) ((num_delete++ & num_staged++)) ;;
      DM) ((num_changed++ & num_delete_cached++ & num_staged++)) ;;
      MD) ((num_changed_cached++ & num_delete++ & num_staged++)) ;;
      AD) ((num_adds++ & num_delete++ & num_staged++)) ;;
      U?) ((num_conflicts++)) ;;
      *) ((num_staged++)) ;;
    esac
  done <<< "$gitstatus"

  # printf 'staged: %s\n' $num_staged
  # printf 'changed %s\n' $num_changed
  # printf 'conflicts: %s\n' $num_conflicts
  # printf 'untracked: %s\n' $num_untracked
  # printf 'delete: %s\n' $num_delete
  # printf 'changed cached: %s\n' $num_changed_cached
  # printf 'delete cached: %s\n' $num_delete_cached
  # printf 'adds: %s\n' $num_adds

  local num_stashed=0
  if [[ $GIT_PROMPT_IGNORE_STASH != 1 ]]; then
    stash_file="$( git rev-parse --git-dir )/logs/refs/stash"
    if [[ -e "${stash_file}" ]]; then
      while IFS='' read -r wcline || [[ -n "$wcline" ]]; do
        ((num_stashed++))
      done < "${stash_file}"
    fi
  fi

  clean=0
  if (( num_changed == 0 && num_staged == 0 && num_untracked == 0 && num_stashed == 0 && num_conflicts == 0)) ; then
    clean=1
  fi

  IFS="^" read -ra branch_fields <<< "${branch_line/\#\# }"
  local branch="${branch_fields[0]}"
  local remote=
  local upstream=
  local ahead=
  local num_ahead=0
  local behind=
  local num_behind=0
  local tag=
  local prehash=
  # http://stackoverflow.com/a/2953673/740527
  local is_gone=false
  local is_detached=false
  local is_tag=false

  if [[ "$branch" == *"Initial commit on"* ]]; then
    IFS=" " read -ra fields <<< "$branch"
    branch="${fields[3]}"
    remote="_NO_REMOTE_TRACKING_"
  elif [[ "$branch" == *"no branch"* ]]; then
    is_detached=true
    prehash=$( git rev-parse --short HEAD 2> /dev/null )
    tag=$( git describe --exact-match 2> /dev/null )
    if [[ -n "$tag" ]]; then
      is_tag=true
      branch="_TAG_${tag}"
    else
      branch="_PREHASH_${prehash}"
    fi
  else
    if [[ "${#branch_fields[@]}" -eq 1 ]]; then
      remote="_NO_REMOTE_TRACKING_"
    else
      IFS="[,]" read -ra remote_fields <<< "${branch_fields[1]}"
      upstream="${remote_fields[0]}"
      for remote_field in "${remote_fields[@]}"; do
        if [[ "$remote_field" == *ahead* ]]; then
          num_ahead=${remote_field:6}
          ahead="_AHEAD_${num_ahead}"
        fi
        if [[ "$remote_field" == *behind* ]]; then
          num_behind=${remote_field:7}
          behind="_BEHIND_${num_behind# }"
        fi
        if [[ "$remote_field" == *gone* ]]; then
          is_gone=true
        fi
      done
      remote="${behind}${ahead}"
    fi
  fi

  if [[ -z "$remote" ]] ; then
    remote='.'
  fi

  if [[ -z "$upstream" ]] ; then
    upstream='^'
  fi

  if [[ -z "$tag" ]] ; then
    tag='.'
  fi

  if [[ -z "$prehash" ]] ; then
    prehash='.'
  fi

  printf "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n" \
         "$branch" \
         "$remote" \
         "$upstream" \
         $num_staged \
         $num_conflicts \
         $num_changed \
         $num_untracked \
         $num_stashed \
         $clean \
         $num_changed_cached \
         $num_delete_cached \
         $num_delete \
         $num_adds \
         $num_ahead \
         $num_behind \
         $is_gone \
         $is_detached \
         $is_tag \
         $tag \
         $prehash
}


# Generic SCM Functions

function scm {
  if [[ "$SCM_CHECK" = false ]]; then SCM=$SCM_NONE
  elif [[ -f .git/HEAD ]]; then SCM=$SCM_GIT
  elif hash git 2> /dev/null && [[ -n "$(git rev-parse --is-inside-work-tree 2> /dev/null)" ]]; then SCM=$SCM_GIT
  elif [[ -d .hg ]]; then SCM=$SCM_HG
  elif hash hg 2> /dev/null && [[ -n "$(hg root 2> /dev/null)" ]]; then SCM=$SCM_HG
  elif [[ -d .svn ]]; then SCM=$SCM_SVN
  else SCM=$SCM_NONE
  fi
}

function scm_prompt_char {
  if [[ -z $SCM ]]; then scm; fi
  if [[ $SCM == "$SCM_GIT" ]]; then SCM_CHAR=$SCM_GIT_CHAR
  elif [[ $SCM == "$SCM_HG" ]]; then SCM_CHAR=$SCM_HG_CHAR
  elif [[ $SCM == "$SCM_SVN" ]]; then SCM_CHAR=$SCM_SVN_CHAR
  else SCM_CHAR=$SCM_NONE_CHAR
  fi
}

function scm_prompt_vars {
  scm
  scm_prompt_char
  SCM_DIRTY=0
  SCM_STATE=''
  [[ $SCM == "$SCM_GIT" ]] && git_prompt_vars && return
  [[ $SCM == "$SCM_HG" ]] && hg_prompt_vars && return
  [[ $SCM == "$SCM_SVN" ]] && svn_prompt_vars && return
}

function scm_prompt_info {
  scm
  scm_prompt_char
  SCM_DIRTY=0
  SCM_STATE=''
  [[ $SCM == "$SCM_GIT" ]] && git_prompt_info && return
  [[ $SCM == "$SCM_HG" ]] && hg_prompt_info && return
  [[ $SCM == "$SCM_SVN" ]] && svn_prompt_info && return
}

# Prompt Functions

function git_prompt_vars {
  if is_function prompt_callback; then
    prompt_callback="prompt_callback"
  else
    prompt_callback="prompt_callback_default"
  fi

  if [[ $GIT_PROMPT_FETCH_REMOTE_STATUS = 1 ]]; then
    check_git_upstream
  fi

  local -a git_status_fields
  git_status_fields=($(git_status 2>/dev/null))

  SCM_GIT_BRANCH=${git_status_fields[0]}
  SCM_GIT_REMOTE=${git_status_fields[1]}
  SCM_GIT_UPSTREAM=${git_status_fields[2]}
  # printf 'branch: %s\n' $SCM_GIT_BRANCH
  # printf 'remote: %s\n' $SCM_GIT_REMOTE
  # printf 'upstream: %s\n' $SCM_GIT_UPSTREAM
  # SCM_GIT_BRANCH=$(replace_git_symbols "$SCM_GIT_BRANCH")
  # SCM_GIT_REMOTE=$(replace_git_symbols "$SCM_GIT_REMOTE")
  # printf 'branch (after): %s\n' $SCM_GIT_BRANCH
  # printf 'remote (after): %s\n' $SCM_GIT_REMOTE

  SCM_GIT_STAGED=${git_status_fields[3]}
  SCM_GIT_CONFLICTS=${git_status_fields[4]}
  SCM_GIT_CHANGED=${git_status_fields[5]}
  SCM_GIT_UNTRACKED=${git_status_fields[6]}
  SCM_GIT_STASHED=${git_status_fields[7]}
  SCM_GIT_CLEAN=${git_status_fields[8]}

  ## need to add their theme partner variable
  SCM_GIT_CHANGED_CACHED=${git_status_fields[9]}
  SCM_GIT_DELETED_CACHED=${git_status_fields[10]}
  SCM_GIT_DELETED=${git_status_fields[11]}
  SCM_GIT_ADDS=${git_status_fields[12]}
  SCM_GIT_AHEAD=${git_status_fields[13]}
  SCM_GIT_BEHIND=${git_status_fields[14]}
  SCM_GIT_GONE=${git_status_fields[15]}
  SCM_GIT_DETACHED=${git_status_fields[16]}
  SCM_GIT_ON_TAG=${git_status_fields[17]}
  SCM_GIT_TAG=${git_status_fields[18]}
  SCM_GIT_PREHASH=${git_status_fields[19]}

  SCM_GIT_ACTION=
  SCM_GIT_UPSTREAM_SHORT=$SCM_GIT_UPSTREAM
  SCM_GIT_HAS_DIVERGED=false
  SCM_GIT_HAS_UPSTREAM=false
  SCM_GIT_SHOULD_PUSH=false
  SCM_GIT_WILL_REBASE=false

  SCM_PREFIX=${GIT_THEME_PROMPT_PREFIX:-$SCM_THEME_PROMPT_PREFIX}
  SCM_SUFFIX=${GIT_THEME_PROMPT_SUFFIX:-$SCM_THEME_PROMPT_SUFFIX}

  if [[ "$SCM_GIT_DETACHED" = true ]]; then
    # SCM_BRANCH="(no branch)"
    SCM_BRANCH=$(replace_git_symbols "$SCM_GIT_BRANCH")
  else
    SCM_BRANCH=$SCM_GIT_BRANCH

    ## Oh My git
    local num_logs
    num_logs=$(git --no-pager log --pretty=oneline -n1 2> /dev/null | wc -l)
    if [[ $num_logs -gt 0 ]]; then
      SCM_GIT_ACTION=$(current_git_action)
      SCM_GIT_WILL_REBASE=$(git config --get branch."${SCM_GIT_BRANCH}".rebase 2> /dev/null)
    fi
  fi

  if [ "$SCM_GIT_ON_TAG" = true ]; then
    SCM_CHANGE=$SCM_GIT_TAG
  else
    SCM_CHANGE=$SCM_GIT_PREHASH
  fi

  # Remote, Upstream

  if [[ -n "$GIT_PROMPT_SHOW_UPSTREAM" && "$SCM_GIT_UPSTREAM" != "^" ]]; then
    SCM_GIT_HAS_UPSTREAM=true

    local arr=(${SCM_GIT_UPSTREAM//\// })
    if [ ${#arr[*]} -eq 2 ]; then
      if [ "${arr[1]}" = "$SCM_GIT_BRANCH" ]; then
        SCM_GIT_UPSTREAM_SHORT=${arr[0]}
      fi
    fi
  fi

  # Ahead, Behind

  if [[ "$SCM_GIT_REMOTE" != "." ]]; then
    if [[ $SCM_GIT_AHEAD -gt 0 && $SCM_GIT_BEHIND -gt 0 ]]; then
      SCM_GIT_HAS_DIVERGED=true
    fi

    if [[ $SCM_GIT_BEHIND -eq 0 && $SCM_GIT_AHEAD -gt 0 ]]; then
      SCM_GIT_SHOULD_PUSH=true
    fi
  fi

  # Clean, Dirty, Staged, etc

  SCM_STATE=${SCM_GIT_PROMPT_CLEAN:-$SCM_THEME_PROMPT_CLEAN}
  if [[ $SCM_GIT_UNTRACKED -gt 0 ]]; then
    SCM_DIRTY=1 # dirty
  elif [[ $SCM_GIT_CHANGED -gt 0 ]]; then
    SCM_DIRTY=2 # un staged
  elif [[ $SCM_GIT_STAGED -gt 0 ]]; then
    SCM_DIRTY=3 # staged
  fi

  if [[ $SCM_DIRTY -gt 0 ]]; then
    SCM_STATE=${GIT_THEME_PROMPT_DIRTY:-$SCM_THEME_PROMPT_DIRTY}
  fi
}

# The prompt may look like the following:
#
# - ``(master↑3|✚1)``: on branch ``master``, ahead of remote by 3 commits, 1
#   file changed but not staged
# - ``(status|●2)``: on branch ``status``, 2 files staged
# - ``(master|✚7…)``: on branch ``master``, 7 files changed, some files
#   untracked
# - ``(master|✖2✚3)``: on branch ``master``, 2 conflicts, 3 files changed
# - ``(master|⚑2)``: on branch ``master``, 2 stash entries
# - ``(experimental↓2↑3|✔)``: on branch ``experimental``; your branch has
#   diverged by 3 commits, remote by 2 commits; the repository is otherwise
#   clean
# - ``(:70c2952|✔)``: not on any branch; parent commit has hash ``70c2952``; the
#   repository is otherwise clean
#
# ##  Prompt Structure
#
# By default, the general appearance of the prompt is::
#
#     (<branch> <upstream branch> <branch tracking>|<local status>)
#
# The symbols are as follows:
#
# - Local Status Symbols
#   - ``✔``: repository clean
#   - ``●n``: there are ``n`` staged files
#   - ``✖n``: there are ``n`` files with merge conflicts
#   - ``✚n``: there are ``n`` changed but *unstaged* files
#   - ``…n``: there are ``n`` untracked files
#   - ``⚑n``: there are ``n`` stash entries
# - Upstream branch
#   - Shows the remote tracking branch
#   - Disabled by default
# - Branch Tracking Symbols
#   - ``↑n``: ahead of remote by ``n`` commits
#   - ``↓n``: behind remote by ``n`` commits
#   - ``↓m↑n``: branches diverged, other by ``m`` commits, yours by ``n`` commits
#   - ``L``: local branch, not remotely tracked
# - Branch Symbol:<br />
#       When the branch name starts with a colon ``:``, it means it's actually a
#       hash, not a branch (although it should be pretty clear, unless you name
#       your branches like hashes :-)
function scm_bash_git_prompt () {
  git_prompt_vars

  SCM_GIT_BRANCH=$(replace_git_symbols "$SCM_GIT_BRANCH")
  SCM_GIT_REMOTE=$(replace_git_symbols "$SCM_GIT_REMOTE")

  local LAST_COMMAND_INDICATOR
  local EMPTY_PROMPT
  local NEW_PROMPT
  local STATUS

  if [[ "." == "$SCM_GIT_REMOTE" ]]; then
    unset SCM_GIT_REMOTE
  fi

  if [[ -z "$GIT_PROMPT_SHOW_UPSTREAM" || "^" == "$SCM_GIT_UPSTREAM" ]]; then
    unset SCM_GIT_UPSTREAM
  else
    SCM_GIT_UPSTREAM="${SCM_GIT_PROMPT_UPSTREAM//_UPSTREAM_/${SCM_GIT_UPSTREAM}}"
  fi

  NEW_PROMPT="$EMPTY_PROMPT"
  if [[ -n "$git_status_fields" ]]; then
    STATUS="${SCM_GIT_PROMPT_PREFIX}"
    STATUS+="${purple}${SCM_GIT_BRANCH}${normal}"

    __chk_gitvar_status() {
      local v
      # printf '%s: %s\n' "$1 $2"
      if [[ "x$2" == "x-n" ]]; then
        v="$2 \"\$SCM_GIT_$1\""
      else
        v="\$SCM_GIT_$1 $2"
        # printf 'v: %s\n' "$v"
      fi
      if eval "test $v" ; then
        if [[ $# -lt 2 || "$3" != '-' ]]; then
          __add_status "\$SCM_GIT_PROMPT_$1\$SCM_GIT_$1\${normal}"
          # printf 's: %s\n' "\$SCM_GIT_PROMPT_$1\$SCM_GIT_$1\${normal}"
        else
          __add_status "\$SCM_GIT_PROMPT_$1\${normal}"
        fi
      fi
    }

    __add_gitvar_status() {
      __add_status "\$SCM_GIT_PROMPT_$1\$SCM_GIT_$1\${normal}"
    }

    # __add_status SOMETEXT
    __add_status() {
      eval "STATUS=\"$STATUS$1\""
    }

    __add_status        "$SCM_GIT_UPSTREAM"
    __chk_gitvar_status 'REMOTE'     '-n'
    __add_status        "$SCM_GIT_PROMPT_SEPARATOR"
    __chk_gitvar_status 'STAGED'     '-ne 0'
    __chk_gitvar_status 'CONFLICTS'  '-ne 0'
    __chk_gitvar_status 'CHANGED'    '-ne 0'
    __chk_gitvar_status 'UNTRACKED'  '-ne 0'
    __chk_gitvar_status 'STASHED'    '-ne 0'
    __chk_gitvar_status 'CLEAN'      '-eq 1'   -
    __add_status        "${normal}$SCM_GIT_PROMPT_SUFFIX"

    # printf 'git remote: %s\n' "${SCM_GIT_REMOTE}"
    # printf 'git staged: %s\n' "${SCM_GIT_STAGED}"
    # printf 'git conflicts: %s\n' "${SCM_GIT_CONFLICTS}"
    # printf 'git changed: %s\n' "${SCM_GIT_CHANGED}"
    # printf 'git untracked: %s\n' "${SCM_GIT_UNTRACKED}"
    # printf 'git stashed: %s\n' "${SCM_GIT_STASHED}"
    # printf 'git clean: %s\n' "${SCM_GIT_CLEAN}"

    # NEW_PROMPT="$($prompt_callback)$STATUS"
    NEW_PROMPT="$STATUS"
  fi

  SCM_PROMPT_GIT="${NEW_PROMPT//_LAST_COMMAND_INDICATOR_/${LAST_COMMAND_INDICATOR}}"
}

function scm_oh_my_git_powerline_prompt {
  git_prompt_vars

  local LAST_COMMAND_INDICATOR

  # on filesystem
  SCM_PROMPT_GIT="${bold_white}${SCM_GIT_CHAR} ${reset_color}"

  if [[ -z $SCM_GIT_ACTION ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_ACTION} "
  fi

  if [[ $SCM_GIT_STASHED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_STASHED//_STASHED_/${SCM_GIT_STASHED}}"
  fi

  if [[ $SCM_GIT_CONFLICTS -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_CONFLICTS//_CONFLICTS_/${SCM_GIT_CONFLICTS}}"
  fi

  if [[ $SCM_GIT_UNTRACKED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_UNTRACKED//_UNTRACKED_/${SCM_GIT_UNTRACKED}}"
  fi

  if [[ $SCM_GIT_CHANGED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_CHANGED//_CHANGED_/${SCM_GIT_CHANGED}}"
  fi

  if [[ $SCM_GIT_DELETED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_DELETED//_DELETED_/${SCM_GIT_DELETED}}"
  fi

  if [[ $SCM_GIT_ADDS -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_ADDS//_ADDS_/${SCM_GIT_ADDS}}"
  fi

  if [[ $SCM_GIT_CHANGED_CACHED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_CHANGED_CACHED//_CHANGED_CACHED_/${SCM_GIT_CHANGED_CACHED}}"
  fi

  if [[ $SCM_GIT_DELETED_CACHED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_DELETED_CACHED//_DELETED_CACHED_/${SCM_GIT_DELETED_CACHED}}"
  fi

  # next operation
  if [[ $SCM_GIT_STAGED -gt 0 ]] && [[ $SCM_GIT_CHANGED -eq 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_STAGED//_STAGED_/${SCM_GIT_STAGED}}"
  fi

  # where
  SCM_PROMPT_GIT="$(set_rgb_color - ${CWD_THEME_PROMPT_COLOR})${SCM_PROMPT_GIT} "
  SCM_PROMPT_GIT+="$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} -)"
  SCM_PROMPT_GIT+="$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} ${SCM_THEME_PROMPT_COLOR})"
  SCM_PROMPT_GIT+="${THEME_PROMPT_SEPARATOR} ${normal}"
  SCM_PROMPT_GIT+="$(set_rgb_color - ${SCM_THEME_PROMPT_COLOR})"

  if [[ "$SCM_GIT_DETACHED" = true ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_PREHASH//_PREHASH_/${SCM_GIT_PREHASH}}"
  else
    if [[ "$SCM_GIT_HAS_UPSTREAM" = true ]]; then
      if [ "$SCM_HAS_DIVERGED" = true ]; then
        SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_BRANCH_DIVERGED//_BEHIND_/${SCM_GIT_BEHIND}}"
        SCM_PROMPT_GIT="${SCM_PROMPT_GIT//_AHEAD_/${SCM_GIT_AHEAD}}"
      else
        if [[ $SCM_GIT_BEHIND -gt 0 ]]; then
          SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_BRANCH_FF//_BEHIND_/${SCM_GIT_BEHIND}}"
        fi
        if [ "$SCM_GIT_SHOULD_PUSH" = true ]; then
          SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_BRANCH_PUSH//_AHEAD_/${SCM_GIT_AHEAD}}"
        fi
        if [[ $SCM_GIT_AHEAD -eq 0 && $SCM_GIT_BEHIND -eq 0 ]]; then
          SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_BRANCH_CLEAN}"
        fi
      fi

      if [[ "$SCM_GIT_WILL_REBASE" = true ]]; then
        SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_REBASE//_BRANCH_/${SCM_GIT_BRANCH}}"
        SCM_PROMPT_GIT="${SCM_PROMPT_GIT//_UPSTREAM_/${SCM_GIT_UPSTREAM_SHORT}}"
      else
        SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_MERGE//_BRANCH_/${SCM_GIT_BRANCH}}"
        SCM_PROMPT_GIT="${SCM_PROMPT_GIT//_UPSTREAM_/${SCM_GIT_UPSTREAM_SHORT}}"
      fi
    else
      SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_NO_REMOTE}"
      SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_NO_UPSTREAM//_BRANCH_/${SCM_GIT_BRANCH}}"
    fi
  fi

  if [ "$SCM_GIT_ON_TAG" = true ]; then
    SCM_PROMPT_GIT+=" ${SCM_GIT_PROMPT_TAG//_TAG_/${SCM_GIT_TAG}}"
  fi
}

function scm_oh_my_git_prompt {
  git_prompt_vars

  local LAST_COMMAND_INDICATOR

  # on filesystem
  SCM_PROMPT_GIT="${bold_white}${SCM_GIT_CHAR} ${reset_color}"

  if [[ -z $SCM_GIT_ACTION ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_ACTION} "
  fi

  if [[ $SCM_GIT_STASHED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_STASHED//_STASHED_/${SCM_GIT_STASHED}}"
  fi

  if [[ $SCM_GIT_CONFLICTS -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_CONFLICTS//_CONFLICTS_/${SCM_GIT_CONFLICTS}}"
  fi

  if [[ $SCM_GIT_UNTRACKED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_UNTRACKED//_UNTRACKED_/${SCM_GIT_UNTRACKED}}"
  fi

  if [[ $SCM_GIT_CHANGED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_CHANGED//_CHANGED_/${SCM_GIT_CHANGED}}"
  fi

  if [[ $SCM_GIT_DELETED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_DELETED//_DELETED_/${SCM_GIT_DELETED}}"
  fi

  if [[ $SCM_GIT_ADDS -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_ADDS//_ADDS_/${SCM_GIT_ADDS}}"
  fi

  if [[ $SCM_GIT_CHANGED_CACHED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_CHANGED_CACHED//_CHANGED_CACHED_/${SCM_GIT_CHANGED_CACHED}}"
  fi

  if [[ $SCM_GIT_DELETED_CACHED -gt 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_DELETED_CACHED//_DELETED_CACHED_/${SCM_GIT_DELETED_CACHED}}"
  fi

  # next operation
  if [[ $SCM_GIT_STAGED -gt 0 ]] && [[ $SCM_GIT_CHANGED -eq 0 ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_STAGED//_STAGED_/${SCM_GIT_STAGED}}"
  fi

  if [[ "$SCM_GIT_DETACHED" = true ]]; then
    SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_PREHASH//_PREHASH_/${SCM_GIT_PREHASH}}"
  else
    if [[ "$SCM_GIT_HAS_UPSTREAM" = true ]]; then
      if [ "$SCM_HAS_DIVERGED" = true ]; then
        SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_BRANCH_DIVERGED//_BEHIND_/${SCM_GIT_BEHIND}}"
        SCM_PROMPT_GIT="${SCM_PROMPT_GIT//_AHEAD_/${SCM_GIT_AHEAD}}"
      else
        if [[ $SCM_GIT_BEHIND -gt 0 ]]; then
          SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_BRANCH_FF//_BEHIND_/${SCM_GIT_BEHIND}}"
        fi
        if [ "$SCM_GIT_SHOULD_PUSH" = true ]; then
          SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_BRANCH_PUSH//_AHEAD_/${SCM_GIT_AHEAD}}"
        fi
        if [[ $SCM_GIT_AHEAD -eq 0 && $SCM_GIT_BEHIND -eq 0 ]]; then
          SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_BRANCH_CLEAN}"
        fi
      fi
      if [[ "$SCM_GIT_WILL_REBASE" = true ]]; then
        SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_REBASE//_BRANCH_/${SCM_GIT_BRANCH}}"
        SCM_PROMPT_GIT="${SCM_PROMPT_GIT//_UPSTREAM_/${SCM_GIT_UPSTREAM}}"
      else
        SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_MERGE//_BRANCH_/${SCM_GIT_BRANCH}}"
        SCM_PROMPT_GIT="${SCM_PROMPT_GIT//_UPSTREAM_/${SCM_GIT_UPSTREAM}}"
      fi
    else
      SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_NO_REMOTE}"
      SCM_PROMPT_GIT+="${SCM_GIT_PROMPT_NO_UPSTREAM//_BRANCH_/${SCM_GIT_BRANCH}}"
    fi
  fi

  if [ "$SCM_GIT_ON_TAG" = true ]; then
    SCM_PROMPT_GIT+=" ${SCM_GIT_PROMPT_TAG//_TAG_/${SCM_GIT_TAG}}"
  fi
}

function scm_powerline_git_prompt {
  git_prompt_vars

  local LAST_COMMAND_INDICATOR

  # Remote, Upstream

  if [ "$SCM_GIT_ON_TAG" = true ]; then
    SCM_PROMPT_GIT=${SCM_THEME_TAG_PREFIX}$SCM_GIT_BRANCH
  elif [ "$SCM_GIT_DETACHED" = true ]; then
    SCM_PROMPT_GIT=${SCM_THEME_DETACHED_PREFIX}$SCM_GIT_BRANCH
  else
    SCM_PROMPT_GIT=$SCM_GIT_BRANCH
  fi

  if [[ -z "$GIT_PROMPT_SHOW_UPSTREAM" || "^" == "$SCM_GIT_UPSTREAM" ]]; then
    unset SCM_GIT_UPSTREAM
  else
    if [ "$SCM_GIT_GONE" = true ]; then
      SCM_PROMPT_GIT+="${SCM_THEME_BRANCH_GONE_PREFIX}${SCM_GIT_UPSTREAM}"
    else
      SCM_PROMPT_GIT+="${SCM_THEME_BRANCH_TRACK_PREFIX}${SCM_GIT_UPSTREAM}"
    fi
  fi

  # Ahead, Behind

  if [[ "." == "$SCM_GIT_REMOTE" ]]; then
    unset SCM_GIT_REMOTE
  else
    if [[ $SCM_GIT_AHEAD -gt 0 ]]; then
      SCM_PROMPT_GIT+=" ${SCM_GIT_PROMPT_SYMBOLS_AHEAD}${SCM_GIT_AHEAD}"
    fi

    if [[ $SCM_GIT_BEHIND -gt 0 ]]; then
      SCM_PROMPT_GIT+=" ${SCM_GIT_PROMPT_SYMBOLS_BEHIND}${SCM_GIT_BEHIND}"
    fi
  fi

  # Stash

  if [[ $SCM_GIT_STASHED -gt 0 ]]; then
    SCM_PROMPT_GIT+=" \{$SCM_GIT_STASHED\}"
  fi

  # Clean, Dirty, Staged, etc

  if [[ $SCM_GIT_STAGED -gt 0 ]]; then
    SCM_PROMPT_GIT+=" ${SCM_GIT_PROMPT_STAGED}${SCM_GIT_STAGED}"
  fi

  if [[ $SCM_GIT_CHANGED -gt 0 ]]; then
    SCM_PROMPT_GIT+=" ${SCM_GIT_PROMPT_CHANGED}${SCM_GIT_CHANGED}"
  fi

  if [[ $SCM_GIT_UNTRACKED -gt 0 ]]; then
    SCM_PROMPT_GIT+=" ${SCM_GIT_PROMPT_UNTRACKED}${SCM_GIT_UNTRACKED}"
  fi

  SCM_PROMPT_GIT="${SCM_PROMPT_GIT//_LAST_COMMAND_INDICATOR_/${LAST_COMMAND_INDICATOR}}"
}

function svn_prompt_vars {
  if [[ -n $(svn status 2> /dev/null) ]]; then
    SCM_DIRTY=1
    SCM_STATE=${SVN_THEME_PROMPT_DIRTY:-$SCM_THEME_PROMPT_DIRTY}
  else
    SCM_DIRTY=0
    SCM_STATE=${SVN_THEME_PROMPT_CLEAN:-$SCM_THEME_PROMPT_CLEAN}
  fi
  SCM_PREFIX=${SVN_THEME_PROMPT_PREFIX:-$SCM_THEME_PROMPT_PREFIX}
  SCM_SUFFIX=${SVN_THEME_PROMPT_SUFFIX:-$SCM_THEME_PROMPT_SUFFIX}
  SCM_BRANCH=$(svn info 2> /dev/null | awk -F/ '/^URL:/ { for (i=0; i<=NF; i++) { if ($i == "branches" || $i == "tags" ) { print $(i+1); break }; if ($i == "trunk") { print $i; break } } }') || return
  SCM_CHANGE=$(svn info 2> /dev/null | sed -ne 's#^Revision: ##p' )
}

# this functions returns absolute location of .hg directory if one exists
# It starts in the current directory and moves its way up until it hits /.
# If we get to / then no Mercurial repository was found.
# Example:
# - lets say we cd into ~/Projects/Foo/Bar
# - .hg is located in ~/Projects/Foo/.hg
# - get_hg_root starts at ~/Projects/Foo/Bar and sees that there is no .hg directory, so then it goes into ~/Projects/Foo
function get_hg_root {
  local CURRENT_DIR
  CURRENT_DIR=$(pwd)

  while [ "$CURRENT_DIR" != "/" ]; do
    if [ -d "$CURRENT_DIR/.hg" ]; then
      echo "$CURRENT_DIR/.hg"
      return
    fi

    CURRENT_DIR=$(dirname "$CURRENT_DIR")
  done
}

function hg_prompt_vars {
  if [[ -n $(hg status 2> /dev/null) ]]; then
    SCM_DIRTY=1
    SCM_STATE=${HG_THEME_PROMPT_DIRTY:-$SCM_THEME_PROMPT_DIRTY}
  else
    SCM_DIRTY=0
    SCM_STATE=${HG_THEME_PROMPT_CLEAN:-$SCM_THEME_PROMPT_CLEAN}
  fi
  SCM_PREFIX=${HG_THEME_PROMPT_PREFIX:-$SCM_THEME_PROMPT_PREFIX}
  SCM_SUFFIX=${HG_THEME_PROMPT_SUFFIX:-$SCM_THEME_PROMPT_SUFFIX}

  HG_ROOT=$(get_hg_root)

  if [ -f "$HG_ROOT/branch" ]; then
    # Mercurial holds it's current branch in .hg/branch file
    SCM_BRANCH=$(cat "${HG_ROOT}/branch")
  else
    SCM_BRANCH=$(hg summary 2> /dev/null | grep branch: | awk '{print $2}')
  fi

  if [ -f "${HG_ROOT}/dirstate" ]; then
    # Mercurial holds various information about the working directory in .hg/dirstate file. More on http://mercurial.selenic.com/wiki/DirState
    SCM_CHANGE=$(hexdump -n 10 -e '1/1 "%02x"' "${HG_ROOT}/dirstate" | cut -c-12)
  else
    SCM_CHANGE=$(hg summary 2> /dev/null | grep parent: | awk '{print $2}')
  fi
}

function rvm_version_prompt {
  if hash rvm 2> /dev/null; then
    rvm=$(rvm tools identifier) || return
    if [ "$rvm" != "system" ]; then
      echo -e "$RVM_THEME_PROMPT_PREFIX$rvm$RVM_THEME_PROMPT_SUFFIX"
    fi
  fi
}

function rbenv_version_prompt {
  if hash rbenv 2> /dev/null; then
    rbenv=$(rbenv version-name) || return
    rbenv commands | grep -q gemset && gemset=$(rbenv gemset active 2> /dev/null) && rbenv="$rbenv@${gemset%% *}"
    if [ "$rbenv" != "system" ]; then
      echo -e "$RBENV_THEME_PROMPT_PREFIX$rbenv$RBENV_THEME_PROMPT_SUFFIX"
    fi
  fi
}

function rbfu_version_prompt {
  if [[ $RBFU_RUBY_VERSION ]]; then
    echo -e "${RBFU_THEME_PROMPT_PREFIX}${RBFU_RUBY_VERSION}${RBFU_THEME_PROMPT_SUFFIX}"
  fi
}

function chruby_version_prompt {
  if declare -f -F chruby &> /dev/null; then
    if declare -f -F chruby_auto &> /dev/null; then
      chruby_auto
    fi

    ruby_version=$(ruby --version | awk '{print $1, $2;}') || return

    if [[ ! $(chruby | grep '*') ]]; then
      ruby_version="${ruby_version} (system)"
    fi
    echo -e "${CHRUBY_THEME_PROMPT_PREFIX}${ruby_version}${CHRUBY_THEME_PROMPT_SUFFIX}"
  fi
}

function ruby_version_prompt {
  echo -e "$(rbfu_version_prompt)$(rbenv_version_prompt)$(rvm_version_prompt)$(chruby_version_prompt)"
}

function virtualenv_prompt {
  if [[ -n "$VIRTUAL_ENV" ]]; then
    virtualenv=$(basename "$VIRTUAL_ENV")
    echo -e "$VIRTUALENV_THEME_PROMPT_PREFIX$virtualenv$VIRTUALENV_THEME_PROMPT_SUFFIX"
  fi
}

function condaenv_prompt {
  if [[ $CONDA_DEFAULT_ENV ]]; then
    echo -e "${CONDAENV_THEME_PROMPT_PREFIX}${CONDA_DEFAULT_ENV}${CONDAENV_THEME_PROMPT_SUFFIX}"
  fi
}

function py_interp_prompt {
  py_version=$(python --version 2>&1 | awk '{print "py-"$2;}') || return
  echo -e "${PYTHON_THEME_PROMPT_PREFIX}${py_version}${PYTHON_THEME_PROMPT_SUFFIX}"
}

function python_version_prompt {
  echo -e "$(virtualenv_prompt)$(condaenv_prompt)$(py_interp_prompt)"
}

# backwards-compatibility
function git_prompt_info {
  git_prompt_vars
  echo -e "$SCM_PREFIX$SCM_BRANCH$SCM_STATE$SCM_SUFFIX"
}

function svn_prompt_info {
  svn_prompt_vars
  echo -e "$SCM_PREFIX$SCM_BRANCH$SCM_STATE$SCM_SUFFIX"
}

function hg_prompt_info() {
  hg_prompt_vars
  echo -e "$SCM_PREFIX$SCM_BRANCH:${SCM_CHANGE#*:}$SCM_STATE$SCM_SUFFIX"
}

function scm_char {
  scm_prompt_char
  echo -e "$SCM_CHAR"
}

function prompt_char {
  scm_char
}

function clock_char {
  local date_string
  if [[ "${THEME_CLOCK_CHECK}" = true ]]; then
    date_string=$(date +"%Y-%m-%d %H:%M:%S")
    echo -e "${bold_cyan:-a}$date_string ${red}$CLOCK_CHAR"
  fi
}

function battery_char {
  if [[ "${THEME_BATTERY_PERCENTAGE_CHECK}" = true ]]; then
    echo -e "${bold_red}$(battery_percentage)%"
  fi
}

# if user has installed battery plugin, skip this...
function battery_charge () {
  # no op
  echo -n
}

function battery_char () {
  # no op
  echo -n
}

function aws_profile {
  if [[ $AWS_DEFAULT_PROFILE ]]; then
    echo -e "${AWS_DEFAULT_PROFILE}"
  else
    echo -e "default"
  fi
}

function set_rgb_color {
  if [[ "${1}" != "-" ]]; then
    fg="38;5;${1}"
  fi
  if [[ "${2}" != "-" ]]; then
    bg="48;5;${2}"
    [[ -n "${fg}" ]] && bg=";${bg}"
  fi
  echo -e "\[\033[${fg}${bg}m\]"
}

# Modified from http://stackoverflow.com/a/1617048/359287
function rootable_limited_pwd() {
  local tilde="~"
  local begin=""           # The nonshortened beginning of the path.
  local short_begin=""     # The shortened beginning of the path.
  local current=""         # The section of the path we're currently working on.
  local in_home=0
  local root_dir="$1"
  local end="${2:-$(pwd)}/"     # The unmodified rest of the path.
  local max_length="${3:-0}"
  local final_max_length="${5:-0}"
  local relative_pwd=""
  local end_basename=""
  local short_current_star
  local sub_current
  local offset

  # treat root dirs as starting point of filesystem, like npm roots
  if [[ ! -z "${root_dir}" ]]; then
    offset=${#root_dir}
    if [[ $offset -gt 0 ]]; then
      end=${end:$offset}
    fi
  elif [[ $end =~ $HOME ]]; then
    in_home=1
    end=${end#$HOME}          #strip /home/username from start of string
    begin=$HOME               #start expansion from the right spot
  fi

  end=${end#/}                # Strip the first /
  local shortened_path=$end   # The whole path, to check the length.
  end_basename=$(basename "${end}")
  max_length=$((max_length-${#end_basename}))

  shopt -q nullglob && NGV="-s" || NGV="-u" # Store the value for later.
  shopt -s nullglob    # Without this, anything that doesn't exist in the filesystem turns into */*/*/...

  while [[ "$end" ]] && (( ${#shortened_path} > max_length ))
  do
    current="${end%%/*}"        # everything before the first /
    end="${end#*/}"             # everything after the first /

    if [[ "${current}" && -z "${end}" ]]; then
      short_current_star="$current"   # No star if we don't shorten it.
    else
      short_current_star="$current"   # No star if we don't shorten it.

      for ((i=${#current}-2; i>=0; i--)); do
        sub_current="${current:0:i}"
        matching=("$begin/$sub_current"*) # Array of all files that start with $sub_current.
        (( ${#matching[*]} != 1 )) && break # Stop shortening if more than one file matches.
        short_current_star="$sub_current*"
      done
    fi

    #advance
    begin="$begin/$current"
    short_begin="$short_begin/$short_current_star"
    shortened_path="$short_begin/$end"
  done

  shortened_path="${shortened_path%/}" # strip trailing /
  shortened_path="${shortened_path#/}" # strip leading /

  if [ $in_home -eq 1 ]; then
    relative_pwd="${tilde}/$shortened_path" #make sure it starts with ~/
  else
    relative_pwd="/$shortened_path" # Make sure it starts with /
  fi
  eval "$4=\$relative_pwd"

  local offset=$((${#relative_pwd}-final_max_length))
  if [ $offset -gt 0 ]; then
    local truncated_relative_pwd=${relative_pwd:$offset:$final_max_length}
    truncated_relative_pwd="…/${truncated_relative_pwd#*/}"
    eval "$6=\$truncated_relative_pwd"
  else
    eval "$6=\$relative_pwd"
  fi

  shopt "$NGV" nullglob # Reset nullglob in case this is being used as a function.
}

# Displays last X characters of pwd
function limited_pwd() {
  # Replace $HOME with ~ if possible
  local tilde="~"
  local relative_pwd="${PWD/#${HOME}/${tilde}}"
  local max_pwd_len=$((${COLUMNS:-80}/3))
  local offset=$((${#relative_pwd}-max_pwd_len))

  if [ $offset -gt 0 ]; then
    local truncated_pwd=${relative_pwd:$offset:$max_pwd_len}
    echo -e "…/${truncated_pwd#*/}"
  else
    echo -e "$relative_pwd"
  fi
}

function find_git_root() {
  local git_root=""

  if [[ $SCM_GIT_CHAR == "$SCM_CHAR" ]]; then
    git_root=$(git rev-parse --git-dir 2>/dev/null)

    if [ -d "$git_root" ]; then
      git_root=$(dirname "$git_root")
      echo "$git_root"
    fi
  fi
}

function findup_npm_root {
  local current_dir
  current_dir=$(pwd)

  while [ "$current_dir" != "/" ]; do
    if [ -f "$current_dir/package.json" ]; then
      echo "$current_dir/package.json"
      return
    elif [ -d "$current_dir/.git" ]; then
      return
    fi

    current_dir=$(dirname "$current_dir")
  done
}

function npm_prompt {
  local npm_name=
  local npm_version=
  local npmre="(@[[:lower:][:digit:]][[:lower:][:digit:]._-]+)\/([[:lower:][:digit:]][[:lower:][:digit:]._-]*)$"
  local result=

  # The "name" field contains your package’s name, and must be lowercase and one
  # word, and may contain hyphens and underscores.
  # /^(@[a-z0-9-~][a-z0-9-._~]*\/)?[a-z0-9-~][a-z0-9-._~]*$/
  if [[ -f "$1" ]] && has_jshon; then
    set -o pipefail && \
      result=($(jshon -Q -e name -u -p -e version -u < "$1"))
    if [ $? -eq 0 ]; then
      npm_version=${result[1]}
      npm_name=$(echo ${result[0]} | cut -d '"' -f2 2> /dev/null)
    else
      npm_version=$(jshon -Q -e version -u < "$1")
      if [ $? -ne 0 ]; then
        npm_version="?"
      fi
      npm_name=$(jshon -Q -e name -u < "$1" | cut -d '"' -f2)
      if [ $? -ne 0 ]; then
        npm_name="?"
      fi
    fi

    if [[ $npm_name =~ $npmre ]]; then
      npm_name="$(echo "$npm_name" | cut -d'/' -f2)"
    fi

    eval "$2=${npm_name}@${npm_version}"
  fi
}

function powerline_npm_version_prompt {
  local npm_prompt
  npm_prompt="$1"

  if [[ ! -z "${npm_prompt}" ]]; then
    NPM_VERSION_PROMPT="$(set_rgb_color - ${NPM_THEME_PROMPT_COLOR}) ${npm_prompt} "
    if [[ "${SEGMENT_AT_RIGHT}" -gt 0 ]]; then
      NPM_VERSION_PROMPT+=$(set_rgb_color ${LAST_THEME_COLOR} ${NPM_THEME_PROMPT_COLOR})${THEME_PROMPT_LEFT_SEPARATOR}${normal}
      (( RIGHT_PROMPT_LENGTH += SEGMENT_AT_RIGHT ))
    fi
    RIGHT_PROMPT_LENGTH=$(( ${RIGHT_PROMPT_LENGTH} + ${#npm_prompt} + 2 ))
    LAST_THEME_COLOR=${NPM_THEME_PROMPT_COLOR}
    (( SEGMENT_AT_RIGHT += 1 ))
  else
    NPM_VERSION_PROMPT=""
  fi
}

function async_run() {
  {
    eval "$@" &> /dev/null
  }&
}
