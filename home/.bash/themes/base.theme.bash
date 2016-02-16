#!/usr/bin/env bash

THEME_PROMPT_HOST='\H'

SCM_CHECK=${SCM_CHECK:=true}

SCM_THEME_PROMPT_DIRTY=' ✗'
SCM_THEME_PROMPT_CLEAN=' ✓'
SCM_THEME_PROMPT_PREFIX=' |'
SCM_THEME_PROMPT_SUFFIX='|'
SCM_THEME_BRANCH_PREFIX=''
SCM_THEME_TAG_PREFIX='tag:'
SCM_THEME_DETACHED_PREFIX='detached:'
SCM_THEME_BRANCH_TRACK_PREFIX=' → '
SCM_THEME_BRANCH_GONE_PREFIX=' ⇢ '

CLOCK_CHAR='☆'
THEME_CLOCK_CHECK=${THEME_CLOCK_CHECK:=true}
THEME_BATTERY_PERCENTAGE_CHECK=${THEME_BATTERY_PERCENTAGE_CHECK:=true}

SCM_GIT_SHOW_DETAILS=${SCM_GIT_SHOW_DETAILS:=true}
SCM_GIT_SHOW_REMOTE_INFO=${SCM_GIT_SHOW_REMOTE_INFO:=auto}
SCM_GIT_IGNORE_UNTRACKED=${SCM_GIT_IGNORE_UNTRACKED:=false}

SCM_GIT='git'
SCM_GIT_CHAR='±'
SCM_GIT_DETACHED_CHAR='⌿'
SCM_GIT_AHEAD_CHAR="↑"
SCM_GIT_BEHIND_CHAR="↓"
SCM_GIT_UNTRACKED_CHAR="?:"
SCM_GIT_UNSTAGED_CHAR="U:"
SCM_GIT_STAGED_CHAR="S:"

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
  if [[ $SCM == $SCM_GIT ]]; then SCM_CHAR=$SCM_GIT_CHAR
  elif [[ $SCM == $SCM_HG ]]; then SCM_CHAR=$SCM_HG_CHAR
  elif [[ $SCM == $SCM_SVN ]]; then SCM_CHAR=$SCM_SVN_CHAR
  else SCM_CHAR=$SCM_NONE_CHAR
  fi
}

function scm_prompt_vars {
  scm
  scm_prompt_char
  SCM_DIRTY=0
  SCM_STATE=''
  [[ $SCM == $SCM_GIT ]] && git_prompt_vars && return
  [[ $SCM == $SCM_HG ]] && hg_prompt_vars && return
  [[ $SCM == $SCM_SVN ]] && svn_prompt_vars && return
}

function scm_prompt_info {
  scm
  scm_prompt_char
  SCM_DIRTY=0
  SCM_STATE=''
  [[ $SCM == $SCM_GIT ]] && git_prompt_info && return
  [[ $SCM == $SCM_HG ]] && hg_prompt_info && return
  [[ $SCM == $SCM_SVN ]] && svn_prompt_info && return
}

function git_prompt_vars {
  local details=''
  SCM_STATE=${GIT_THEME_PROMPT_CLEAN:-$SCM_THEME_PROMPT_CLEAN}
  if [[ "$(git config --get bash-it.hide-status)" != "1" ]]; then
    [[ "${SCM_GIT_IGNORE_UNTRACKED}" = "true" ]] && local git_status_flags='-uno'
    local status="$(git status -b --porcelain ${git_status_flags} 2> /dev/null ||
                    git status --porcelain ${git_status_flags} 2> /dev/null)"
    if [[ -n "${status}" ]] && [[ "${status}" != "\n" ]] && [[ -n "$(grep -v ^# <<< "${status}")" ]]; then
      SCM_DIRTY=1
      if [[ "${SCM_GIT_SHOW_DETAILS}" = "true" ]]; then
        local untracked_count="$(egrep -c '^\?\? .+' <<< "${status}")"
        local unstaged_count="$(egrep -c '^.[^ ?#] .+' <<< "${status}")"
        local staged_count="$(egrep -c '^[^ ?#]. .+' <<< "${status}")"
        [[ "${staged_count}" -gt 0 ]] && details+=" ${SCM_GIT_STAGED_CHAR}${staged_count}" && SCM_DIRTY=3
        [[ "${unstaged_count}" -gt 0 ]] && details+=" ${SCM_GIT_UNSTAGED_CHAR}${unstaged_count}" && SCM_DIRTY=2
        [[ "${untracked_count}" -gt 0 ]] && details+=" ${SCM_GIT_UNTRACKED_CHAR}${untracked_count}" && SCM_DIRTY=1
      fi
      SCM_STATE=${GIT_THEME_PROMPT_DIRTY:-$SCM_THEME_PROMPT_DIRTY}
    fi
  fi

  SCM_CHANGE=$(git rev-parse --short HEAD 2>/dev/null)

  local ref=$(git symbolic-ref -q HEAD 2> /dev/null)
  if [[ -n "$ref" ]]; then
    SCM_BRANCH=${SCM_THEME_BRANCH_PREFIX}${ref#refs/heads/}
    local tracking_info="$(grep "${SCM_BRANCH}\.\.\." <<< "${status}")"
    if [[ -n "${tracking_info}" ]]; then
      [[ "${tracking_info}" =~ .+\[gone\]$ ]] && local branch_gone="true"
      tracking_info=${tracking_info#\#\# ${SCM_BRANCH}...}
      tracking_info=${tracking_info% [*}
      local remote_name=${tracking_info%%/*}
      local remote_branch=${tracking_info#${remote_name}/}
      local remote_info=""
      local num_remotes=$(git remote | wc -l 2> /dev/null)
      [[ "${SCM_BRANCH}" = "${remote_branch}" ]] && local same_branch_name=true
      if ([[ "${SCM_GIT_SHOW_REMOTE_INFO}" = "auto" ]] && [[ "${num_remotes}" -ge 2 ]]) ||
          [[ "${SCM_GIT_SHOW_REMOTE_INFO}" = "true" ]]; then
        remote_info="${remote_name}"
        [[ "${same_branch_name}" != "true" ]] && remote_info+="/${remote_branch}"
      elif [[ ${same_branch_name} != "true" ]]; then
        remote_info="${remote_branch}"
      fi
      if [[ -n "${remote_info}" ]];then
        if [[ "${branch_gone}" = "true" ]]; then
          SCM_BRANCH+="${SCM_THEME_BRANCH_GONE_PREFIX}${remote_info}"
        else
          SCM_BRANCH+="${SCM_THEME_BRANCH_TRACK_PREFIX}${remote_info}"
        fi
      fi
    fi
    SCM_GIT_DETACHED="false"
  else
    local detached_prefix=""
    ref=$(git describe --tags --exact-match 2> /dev/null)
    if [[ -n "$ref" ]]; then
      detached_prefix=${SCM_THEME_TAG_PREFIX}
    else
      ref=$(git describe --contains --all HEAD 2> /dev/null)
      ref=${ref#remotes/}
      [[ -z "$ref" ]] && ref=${SCM_CHANGE}
      detached_prefix=${SCM_THEME_DETACHED_PREFIX}
    fi
    SCM_BRANCH=${detached_prefix}${ref}
    SCM_GIT_DETACHED="true"
  fi

  local ahead_re='.+ahead ([0-9]+).+'
  local behind_re='.+behind ([0-9]+).+'
  [[ "${status}" =~ ${ahead_re} ]] && SCM_BRANCH+=" ${SCM_GIT_AHEAD_CHAR}${BASH_REMATCH[1]}"
  [[ "${status}" =~ ${behind_re} ]] && SCM_BRANCH+=" ${SCM_GIT_BEHIND_CHAR}${BASH_REMATCH[1]}"

  local stash_count="$(git stash list 2> /dev/null | wc -l | tr -d ' ')"
  [[ "${stash_count}" -gt 0 ]] && SCM_BRANCH+=" {${stash_count}}"

  SCM_BRANCH+=${details}

  SCM_PREFIX=${GIT_THEME_PROMPT_PREFIX:-$SCM_THEME_PROMPT_PREFIX}
  SCM_SUFFIX=${GIT_THEME_PROMPT_SUFFIX:-$SCM_THEME_PROMPT_SUFFIX}

  ## oh my git

  deteached=
  just_init=
  has_upstream=
  has_modifications=
  has_modifications_cached=
  has_adds=
  has_deletions=
  has_deletions_cached=
  ready_to_commit=
  has_untracked_files=
  is_on_a_tag=
  has_diverged=
  should_push=
  has_stashes=
  will_rebase=

  current_commit_hash=$(git rev-parse HEAD 2> /dev/null)

  current_branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
  if [[ $current_branch == 'HEAD' ]]; then detached=true; fi

  local number_of_logs="$(git log --pretty=oneline -n1 2> /dev/null | wc -l)"
  if [[ $number_of_logs -eq 0 ]]; then
      just_init=true
  else
      local upstream=$(git rev-parse --symbolic-full-name --abbrev-ref @{upstream} 2> /dev/null)
      if [[ -n "${upstream}" && "${upstream}" != "@{upstream}" ]]; then has_upstream=true; fi

      local git_status="$(git status --porcelain 2> /dev/null)"
      action="$(get_current_action)"

      if [[ $git_status =~ ($'\n'|^).M ]]; then has_modifications=true; fi
      if [[ $git_status =~ ($'\n'|^)M ]]; then has_modifications_cached=true; fi
      if [[ $git_status =~ ($'\n'|^)A ]]; then has_adds=true; fi
      if [[ $git_status =~ ($'\n'|^).D ]]; then has_deletions=true; fi
      if [[ $git_status =~ ($'\n'|^)D ]]; then has_deletions_cached=true; fi
      if [[ $git_status =~ ($'\n'|^)[MAD] && ! $git_status =~ ($'\n'|^).[MAD\?] ]]; then ready_to_commit=true; fi

      local number_of_untracked_files=$(\grep -c "^??" <<< "${git_status}")
      if [[ $number_of_untracked_files -gt 0 ]]; then has_untracked_files=true; fi

      tag_at_current_commit=$(git describe --exact-match --tags $current_commit_hash 2> /dev/null)
      if [[ -n $tag_at_current_commit ]]; then is_on_a_tag=true; fi

      if [[ $has_upstream == true ]]; then
          local commits_diff="$(git log --pretty=oneline --topo-order --left-right ${current_commit_hash}...${upstream} 2> /dev/null)"
          commits_ahead=$(\grep -c "^<" <<< "$commits_diff")
          commits_behind=$(\grep -c "^>" <<< "$commits_diff")
      fi

      if [[ $commits_ahead -gt 0 && $commits_behind -gt 0 ]]; then has_diverged=true; fi
      if [[ $has_diverged == false && $commits_ahead -gt 0 ]]; then should_push=true; fi

      will_rebase=$(git config --get branch.${current_branch}.rebase 2> /dev/null)

      local number_of_stashes="$(git stash list -n1 2> /dev/null | wc -l)"
      if [[ $number_of_stashes -gt 0 ]]; then has_stashes=true; fi
  fi
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
    local CURRENT_DIR=$(pwd)

    while [ "$CURRENT_DIR" != "/" ]; do
        if [ -d "$CURRENT_DIR/.hg" ]; then
            echo "$CURRENT_DIR/.hg"
            return
        fi

        CURRENT_DIR=$(dirname $CURRENT_DIR)
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

    if [ -f $HG_ROOT/branch ]; then
        # Mercurial holds it's current branch in .hg/branch file
        SCM_BRANCH=$(cat $HG_ROOT/branch)
    else
        SCM_BRANCH=$(hg summary 2> /dev/null | grep branch: | awk '{print $2}')
    fi

    if [ -f $HG_ROOT/dirstate ]; then
        # Mercurial holds various information about the working directory in .hg/dirstate file. More on http://mercurial.selenic.com/wiki/DirState
        SCM_CHANGE=$(hexdump -n 10 -e '1/1 "%02x"' $HG_ROOT/dirstate | cut -c-12)
    else
        SCM_CHANGE=$(hg summary 2> /dev/null | grep parent: | awk '{print $2}')
    fi
}

function rvm_version_prompt {
  if hash rvm 2> /dev/null; then
    rvm=$(rvm tools identifier) || return
    if [ $rvm != "system" ]; then
      echo -e "$RVM_THEME_PROMPT_PREFIX$rvm$RVM_THEME_PROMPT_SUFFIX"
    fi
  fi
}

function rbenv_version_prompt {
  if hash rbenv 2> /dev/null; then
    rbenv=$(rbenv version-name) || return
    $(rbenv commands | grep -q gemset) && gemset=$(rbenv gemset active 2> /dev/null) && rbenv="$rbenv@${gemset%% *}"
    if [ $rbenv != "system" ]; then
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
    virtualenv=`basename "$VIRTUAL_ENV"`
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
    if [[ "${THEME_CLOCK_CHECK}" = true ]]; then
        DATE_STRING=$(date +"%Y-%m-%d %H:%M:%S")
        echo -e "${bold_cyan}$DATE_STRING ${red}$CLOCK_CHAR"
    fi
}

function battery_char {
    if [[ "${THEME_BATTERY_PERCENTAGE_CHECK}" = true ]]; then
        echo -e "${bold_red}$(battery_percentage)%"
    fi
}

# if user has installed battery plugin, skip this...
function battery_charge (){
# no op
echo -n
}

function battery_char (){
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

# from oh-my-git
function get_current_action () {
  local info="$(git rev-parse --git-dir 2>/dev/null)"
  if [ -n "$info" ]; then
    local action
    if [ -f "$info/rebase-merge/interactive" ]
    then
      action=${is_rebasing_interactively:-"rebase -i"}
    elif [ -d "$info/rebase-merge" ]
    then
      action=${is_rebasing_merge:-"rebase -m"}
    else
      if [ -d "$info/rebase-apply" ]
      then
        if [ -f "$info/rebase-apply/rebasing" ]
        then
          action=${is_rebasing:-"rebase"}
        elif [ -f "$info/rebase-apply/applying" ]
        then
          action=${is_applying_mailbox_patches:-"am"}
        else
          action=${is_rebasing_mailbox_patches:-"am/rebase"}
        fi
      elif [ -f "$info/MERGE_HEAD" ]
      then
        action=${is_merging:-"merge"}
      elif [ -f "$info/CHERRY_PICK_HEAD" ]
      then
        action=${is_cherry_picking:-"cherry-pick"}
      elif [ -f "$info/BISECT_LOG" ]
      then
        action=${is_bisecting:-"bisect"}
      fi
    fi

    if [[ -n $action ]]; then printf "%s" "${1-}$action${2-}"; fi
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
  local begin='' # The unshortened beginning of the path.
  local shortbegin='' # The shortened beginning of the path.
  local current='' # The section of the path we're currently working on.
  local INHOME=0
  local root_dir="$1"
  local end="${2:-$(pwd)}/" # The unmodified rest of the path.
  local maxlength="${3:-0}"
  local final_max_length="${5:-0}"
  local relative_pwd=''
  local end_basename=''

  # treat root dirs as starting point of filesystem, like npm roots
  if [[ ! -z "${root_dir}" ]]; then
    local offset=${#root_dir}
    if [ $offset -gt "0" ]; then
      end=${end:$offset}
    fi
  elif [[ "$end" =~ $HOME ]]; then
    INHOME=1
    end="${end#$HOME}" #strip /home/username from start of string
    begin="$HOME"      #start expansion from the right spot
  fi

  end="${end#/}" # Strip the first /
  local shortenedpath="$end" # The whole path, to check the length.
  end_basename=$(basename "${end}")
  maxlength=$(($maxlength-${#end_basename}))

  shopt -q nullglob && NGV="-s" || NGV="-u" # Store the value for later.
  shopt -s nullglob    # Without this, anything that doesn't exist in the filesystem turns into */*/*/...

  while [[ "$end" ]] && (( ${#shortenedpath} > maxlength ))
  do
    current="${end%%/*}" # everything before the first /
    end="${end#*/}"    # everything after the first /

    if [[ "${current}" && -z "${end}" ]]; then
      shortcurstar="$current" # No star if we don't shorten it.
    else
      shortcurstar="$current" # No star if we don't shorten it.

      for ((i=${#current}-2; i>=0; i--)); do
        subcurrent="${current:0:i}"
        matching=("$begin/$subcurrent"*) # Array of all files that start with $subcurrent.
        (( ${#matching[*]} != 1 )) && break # Stop shortening if more than one file matches.
        shortcurstar="$subcurrent*"
      done
    fi

    #advance
    begin="$begin/$current"
    shortbegin="$shortbegin/$shortcurstar"
    shortenedpath="$shortbegin/$end"
  done

  shortenedpath="${shortenedpath%/}" # strip trailing /
  shortenedpath="${shortenedpath#/}" # strip leading /

  if [ $INHOME -eq 1 ]; then
    relative_pwd="~/$shortenedpath" #make sure it starts with ~/
  else
    relative_pwd="/$shortenedpath" # Make sure it starts with /
  fi
  eval "$4=\$relative_pwd"
  # eval "$4='$relative_pwd'"
  # eval "$4='"${relative_pwd//\'/\'\"\'\"\'}"'"

  local offset=$((${#relative_pwd}-$final_max_length))
  if [ $offset -gt "0" ]
  then
    local truncated_rel=${relative_pwd:$offset:$final_max_length}
    truncated_rel="${TRUNCATED_SYMBOL}/${truncated_rel#*/}"
    eval "$6=\$truncated_rel"
  else
    eval "$6=\$relative_pwd"
  fi

  shopt "$NGV" nullglob # Reset nullglob in case this is being used as a function.
}

# Max length of PWD to display
MAX_PWD_LENGTH=24

# Displays last X characters of pwd
function limited_pwd() {

  # Replace $HOME with ~ if possible
  RELATIVE_PWD=${PWD/#$HOME/\~}

  local offset=$((${#RELATIVE_PWD}-$MAX_PWD_LENGTH))

  if [ $offset -gt "0" ]
  then
    local truncated_symbol="..."
    TRUNCATED_PWD=${RELATIVE_PWD:$offset:$MAX_PWD_LENGTH}
    echo -e "${truncated_symbol}/${TRUNCATED_PWD#*/}"
  else
    echo -e "${RELATIVE_PWD}"
  fi
}
