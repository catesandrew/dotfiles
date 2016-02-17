#!/usr/bin/env bash

THEME_PROMPT_SEPARATOR=""
THEME_PROMPT_LEFT_SEPARATOR=""

SHELL_SSH_CHAR=${SHELL_SSH_CHAR:=" "}
SHELL_THEME_PROMPT_COLOR=32
SHELL_THEME_PROMPT_COLOR_SUDO=202

VIRTUALENV_CHAR=${POWERLINE_VIRTUALENV_CHAR:="❲p❳ "}
CONDA_VIRTUALENV_CHAR=${POWERLINE_CONDA_VIRTUALENV_CHAR:="❲c❳ "}
VIRTUALENV_THEME_PROMPT_COLOR=35

SCM_NONE_CHAR=""
SCM_GIT_CHAR=${POWERLINE_SCM_GIT_CHAR:=" "}
PROMPT_CHAR=${POWERLINE_PROMPT_CHAR:="❯"}
TRUNCATED_SYMBOL="…"

SCM_THEME_PROMPT_CLEAN=""
SCM_THEME_PROMPT_DIRTY=""

SCM_THEME_PROMPT_CLEAN_COLOR=25
SCM_THEME_PROMPT_DIRTY_COLOR=88
SCM_THEME_PROMPT_STAGED_COLOR=30
SCM_THEME_PROMPT_UNSTAGED_COLOR=92
SCM_THEME_PROMPT_COLOR=${SCM_THEME_PROMPT_CLEAN_COLOR}

RVM_THEME_PROMPT_PREFIX=""
RVM_THEME_PROMPT_SUFFIX=""
RVM_THEME_PROMPT_COLOR=161
RVM_CHAR=${POWERLINE_RVM_CHAR:="❲r❳ "}

NPM_THEME_PROMPT_COLOR=20

CWD_THEME_PROMPT_COLOR=240

LAST_STATUS_THEME_PROMPT_COLOR=196

CLOCK_THEME_PROMPT_COLOR=240

BATTERY_AC_CHAR="⚡"
BATTERY_STATUS_THEME_PROMPT_GOOD_COLOR=70
BATTERY_STATUS_THEME_PROMPT_LOW_COLOR=208
BATTERY_STATUS_THEME_PROMPT_CRITICAL_COLOR=160

THEME_PROMPT_CLOCK_FORMAT=${THEME_PROMPT_CLOCK_FORMAT:="%H:%M:%S"}

function powerline_shell_prompt {
    SHELL_PROMPT_COLOR=${SHELL_THEME_PROMPT_COLOR}
    CAN_I_RUN_SUDO=$(sudo -n uptime 2>&1 | grep "load" | wc -l)
    if [ ${CAN_I_RUN_SUDO} -gt 0 ]; then
        SHELL_PROMPT_COLOR=${SHELL_THEME_PROMPT_COLOR_SUDO}
    fi
    SEGMENT_AT_RIGHT=0
    if [[ -n "${SSH_CLIENT}" ]]; then
        SHELL_PROMPT="${SHELL_SSH_CHAR}${USER}@${HOSTNAME}"
    else
        SHELL_PROMPT=""
    fi
    RIGHT_PROMPT_LENGTH=$(( ${RIGHT_PROMPT_LENGTH} + ${#SHELL_PROMPT} + 2 ))
    SHELL_PROMPT="$(set_rgb_color - ${SHELL_PROMPT_COLOR}) ${SHELL_PROMPT} ${normal}"
    LAST_THEME_COLOR=${SHELL_PROMPT_COLOR}
    (( SEGMENT_AT_RIGHT += 1 ))
}

function powerline_rvm_prompt {
    local environ=""

    if command_exists rvm; then
        rvm_prompt=$(rvm_version_prompt)
        if [[ "${rvm_prompt}" != $(rvm strings default) ]]; then
            RVM_PROMPT="$(set_rgb_color - ${RVM_THEME_PROMPT_COLOR}) ${RVM_CHAR}${rvm_prompt} ${normal}"
            if [[ "${SEGMENT_AT_LEFT}" -gt 0 ]]; then
                RVM_PROMPT=$(set_rgb_color ${LAST_THEME_COLOR} ${RVM_THEME_PROMPT_COLOR})${THEME_PROMPT_SEPARATOR}${normal}${RVM_PROMPT}
            fi
            LAST_THEME_COLOR=${RVM_THEME_PROMPT_COLOR}
            (( SEGMENT_AT_LEFT += 1 ))
        else
            RVM_PROMPT=""
        fi
    fi
}

function powerline_virtualenv_prompt {
    local environ=""

    if [[ -n "$CONDA_DEFAULT_ENV" ]]; then
        environ="$CONDA_DEFAULT_ENV"
        VIRTUALENV_CHAR=${CONDA_VIRTUALENV_CHAR}
    elif [[ -n "$VIRTUAL_ENV" ]]; then
        environ=$(basename "$VIRTUAL_ENV")
    fi

    if [[ -n "$environ" ]]; then
        VIRTUALENV_PROMPT="$(set_rgb_color - ${VIRTUALENV_THEME_PROMPT_COLOR}) ${VIRTUALENV_CHAR}$environ ${normal}"
        if [[ "${SEGMENT_AT_LEFT}" -gt 0 ]]; then
            VIRTUALENV_PROMPT=$(set_rgb_color ${LAST_THEME_COLOR} ${VIRTUALENV_THEME_PROMPT_COLOR})${THEME_PROMPT_SEPARATOR}${normal}${VIRTUALENV_PROMPT}
        fi
        LAST_THEME_COLOR=${VIRTUALENV_THEME_PROMPT_COLOR}
        (( SEGMENT_AT_LEFT += 1 ))
    else
        VIRTUALENV_PROMPT=""
    fi
}

function powerline_scm_prompt {
  local omg_is_a_git_repo_symbol=''
  local omg_has_untracked_files_symbol=''        #                ?    
  local omg_has_adds_symbol=''
  local omg_has_deletions_symbol=''
  local omg_has_cached_deletions_symbol=''
  local omg_has_modifications_symbol=''
  local omg_has_cached_modifications_symbol=''
  local omg_ready_to_commit_symbol=''            #   →
  local omg_is_on_a_tag_symbol=''                #   
  # local omg_needs_to_merge_symbol='ᄉ'
  local omg_detached_symbol=''
  local omg_can_fast_forward_symbol=''
  local omg_has_diverged_symbol=''               #   
  local omg_not_tracked_branch_symbol=''
  local omg_rebase_tracking_branch_symbol=''     #   
  local omg_merge_tracking_branch_symbol=''      #  
  local omg_should_push_symbol=''                #    
  local omg_has_stashes_symbol=''
  local omg_space='　'

  scm_prompt_vars
  if [[ "${SCM_NONE_CHAR}" != "${SCM_CHAR}" ]]; then
    if [[ "${SCM_DIRTY}" -eq 3 ]]; then
      SCM_THEME_PROMPT_COLOR=${SCM_THEME_PROMPT_STAGED_COLOR}
    elif [[ "${SCM_DIRTY}" -eq 2 ]]; then
      SCM_THEME_PROMPT_COLOR=${SCM_THEME_PROMPT_UNSTAGED_COLOR}
    elif [[ "${SCM_DIRTY}" -eq 1 ]]; then
      SCM_THEME_PROMPT_COLOR=${SCM_THEME_PROMPT_DIRTY_COLOR}
    else
      SCM_THEME_PROMPT_COLOR=${SCM_THEME_PROMPT_CLEAN_COLOR}
    fi

    if [[ "${SCM_GIT_CHAR}" == "${SCM_CHAR}" ]]; then
      # on filesystem
      SCM_PROMPT="${omg_is_a_git_repo_symbol}${omg_space}"

      if [[ $SCM_ACTION ]]; then
        SCM_PROMPT+="${SCM_ACTION}${omg_space}"
      fi

      if [[ $SCM_HAS_STASHES ]]; then
        SCM_PROMPT+="${omg_has_stashes_symbol}${omg_space}"
      fi

      if [[ $SCM_HAS_UNTRACKED_FILES ]]; then
        SCM_PROMPT+="${omg_has_untracked_files_symbol}${omg_space}"
      fi

      if [[ $SCM_HAS_MODIFICATIONS ]]; then
        SCM_PROMPT+="${omg_has_modifications_symbol}"
      fi

      if [[ $SCM_HAS_DELETIONS ]]; then
        SCM_PROMPT+="${omg_has_deletions_symbol}${omg_space}"
      fi

      # ready
      if [[ $SCM_HAS_ADDS ]]; then
        SCM_PROMPT+="${omg_has_adds_symbol}${omg_space}"
      fi

      if [[ $SCM_HAS_MODIFICATIONS_CACHED ]]; then
        SCM_PROMPT+="${omg_has_cached_modifications_symbol}${omg_space}"
      fi

      if [[ $SCM_HAS_DELETIONS_CACHED ]]; then
        SCM_PROMPT+="${omg_has_cached_deletions_symbol}${omg_space}"
      fi

      # next operation
      if [[ $SCM_READY_TO_COMMIT ]]; then
        SCM_PROMPT+="${omg_ready_to_commit_symbol}${omg_space}"
      fi

      # where
      SCM_PROMPT="$(set_rgb_color - ${CWD_THEME_PROMPT_COLOR})${SCM_PROMPT}  ${normal}"
      SCM_PROMPT+="$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} -)${normal}"
      SCM_PROMPT+="$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} ${SCM_THEME_PROMPT_COLOR})"
      SCM_PROMPT+="${THEME_PROMPT_SEPARATOR} ${normal}"
      SCM_PROMPT+="$(set_rgb_color - ${SCM_THEME_PROMPT_COLOR})"

      if [[ $SCM_GIT_DETACHED == true ]]; then
        SCM_PROMPT+="${omg_detached_symbol}${omg_space}"
        SCM_PROMPT+="(${SCM_CHANGE})${omg_space}"
      else
        if [[ $SCM_HAS_UPSTREAM == false ]]; then
          SCM_PROMPT+="—${omg_space}${omg_not_tracked_branch_symbol}"
          SCM_PROMPT+="${omg_space}—${omg_space}(${SCM_CURRENT_BRANCH})${omg_space}"
        else
          if [[ $SCM_WILL_REBASE == true ]]; then
            local type_of_upstream=$omg_rebase_tracking_branch_symbol
          else
            local type_of_upstream=$omg_merge_tracking_branch_symbol
          fi

          if [[ $SCM_HAS_DIVERGED == true ]]; then
            SCM_PROMPT+="-${SCM_COMMITS_BEHIND}${omg_space}"
            SCM_PROMPT+="${omg_has_diverged_symbol}${omg_space}+${SCM_COMMITS_AHEAD}"
          else
            if [[ $SCM_COMMITS_BEHIND -gt 0 ]]; then
              SCM_PROMPT+="-${SCM_COMMITS_BEHIND}${omg_space}${omg_can_fast_forward_symbol}${omg_space}—"
            fi
            if [[ $SCM_SHOULD_PUSH == true ]]; then
              SCM_PROMPT+="—${omg_space}${omg_should_push_symbol}${omg_space}+${SCM_COMMITS_AHEAD}"
            fi
            if [[ $SCM_COMMITS_AHEAD == 0 && $SCM_COMMITS_BEHIND == 0 ]]; then
              SCM_PROMPT+="${omg_space}—${omg_space}—${omg_space}"
            fi

          fi
          SCM_PROMPT+=" (${SCM_CURRENT_BRANCH} ${type_of_upstream} ${upstream//\/$SCM_CURRENT_BRANCH/}) "
        fi
      fi
      if [[ ${SCM_IS_ON_A_TAG} ]]; then SCM_PROMPT+="${omg_is_on_a_tag_symbol} ${tag_at_current_commit} "; fi
    fi

    SCM_PROMPT="$(set_rgb_color - ${SCM_THEME_PROMPT_COLOR})${SCM_PROMPT} ${normal}"
    LAST_THEME_COLOR=${SCM_THEME_PROMPT_COLOR}
    (( SEGMENT_AT_LEFT += 1 ))
  else
    SCM_PROMPT=""
  fi
}

function powerline_cwd_prompt {
  local npm_root=''
  local npm_prompt=''
  local short_pwd=''
  local trunc_short_pwd=''
  local working_dir=''
  local sud_sorking_dir=''
  local git_dir=''
  local sub_git_dir=''

  npm_root="$1"
  npm_prompt="$2"
  git_dir="$3"
  working_dir=$(pwd)
  sub_working_dir="${working_dir##*/}"

  if [[ ! -z "${git_dir}" ]]; then
    sub_git_dir="${git_dir##*/}"
  fi

  if [[ ! -z "${npm_root}" ]]; then
    # strip the package.json filename
    npm_root=$(dirname "${npm_root}")
    rootable_limited_pwd "$npm_root" "$working_dir" 24 short_pwd 30 trunc_short_pwd
  else
    rootable_limited_pwd '' "$working_dir" 24 short_pwd 30 trunc_short_pwd
  fi

  if [[ ! -z "${npm_root}" ]]; then
      title "${npm_prompt}"
  elif [[ ! -z "${sub_git_dir}" ]]; then
      title "${sub_git_dir}"
  else
      title "${short_pwd}"
      # real_short_pwd=`realpath ${short_pwd}`
      # title `basename ${real_short_pwd}`
  fi

  CWD_PROMPT="$(set_rgb_color - ${CWD_THEME_PROMPT_COLOR}) ${trunc_short_pwd} ${normal}$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} -)${normal}$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} -)${THEME_PROMPT_SEPARATOR}${normal}"
  if [[ "${SEGMENT_AT_LEFT}" -gt 0 ]]; then
    CWD_PROMPT=$(set_rgb_color ${LAST_THEME_COLOR} ${CWD_THEME_PROMPT_COLOR})${THEME_PROMPT_SEPARATOR}${normal}${CWD_PROMPT}
    SEGMENT_AT_LEFT=0
  fi
  LAST_THEME_COLOR=${CWD_THEME_PROMPT_COLOR}
}

function powerline_last_status_prompt {
    if [[ "$1" -eq 0 ]]; then
        LAST_STATUS_PROMPT=""
    else
        LAST_STATUS_PROMPT="$(set_rgb_color ${LAST_STATUS_THEME_PROMPT_COLOR} -) ${LAST_STATUS} ${normal}"
    fi
}

function powerline_clock_prompt {
    if [[ -z "${THEME_PROMPT_CLOCK_FORMAT}" ]]; then
        CLOCK_PROMPT=""
    else
        local CLOCK=" $(date +"${THEME_PROMPT_CLOCK_FORMAT}") "

        CLOCK_PROMPT=$(set_rgb_color - ${CLOCK_THEME_PROMPT_COLOR})${CLOCK}${normal}
        if [[ "${SEGMENT_AT_RIGHT}" -gt 0 ]]; then
            CLOCK_PROMPT+=$(set_rgb_color ${LAST_THEME_COLOR} ${CLOCK_THEME_PROMPT_COLOR})${THEME_PROMPT_LEFT_SEPARATOR}${normal}
            (( RIGHT_PROMPT_LENGTH += SEGMENT_AT_RIGHT - 1 ))
        fi
        RIGHT_PROMPT_LENGTH=$(( ${RIGHT_PROMPT_LENGTH} + ${#CLOCK} ))
        LAST_THEME_COLOR=${CLOCK_THEME_PROMPT_COLOR}
        (( SEGMENT_AT_RIGHT += 1 ))
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

function find_git_root {
  git_root=''

  if [[ "${SCM_GIT_CHAR}" == "${SCM_CHAR}" ]]; then
    git_root="$(git rev-parse --git-dir 2>/dev/null)"

    if [ -d "$git_root" ]; then
      git_root=$(dirname "$git_root")
      echo "$git_root"
    fi
  fi
}

function npm_prompt {
  local npm_root
  local npm_name
  local npm_version
  local package_json

  if hash JSON.sh 2> /dev/null; then
    npm_root="$1"

    if [[ -f "${npm_root}" ]]; then
      package_json=$(JSON.sh < "${npm_root}")
      npm_version=$(echo "${package_json}" | grep -e '^\[\"version\"\]' | cut -d " " -f2 | awk -F\" '{print $(NF-1)}')
      npm_name=$(echo "${package_json}" | grep -e '^\[\"name\"\]' | cut -d " " -f2 | awk -F\" '{print $(NF-1)}')
      eval "$2=${npm_name}@${npm_version}"
    fi
  fi
}

function powerline_npm_version_prompt {
  # if hash npm 2> /dev/null; then
  #   npm_prompt="$(npm show . version 2> /dev/null)"

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

function powerline_prompt_command() {
    RIGHT_PROMPT_LENGTH=1
    local LAST_STATUS="$?"
    local MOVE_CURSOR_RIGHTMOST='\033[500C'
    local local_npm_root
    local local_git_root
    local local_npm_prompt

    local_npm_root="$(findup_npm_root)"
    npm_prompt "$local_npm_root" local_npm_prompt

    ## left prompt ##
    powerline_scm_prompt
    local_git_root="$(find_git_root)"

    # powerline_virtualenv_prompt
    # powerline_rvm_prompt
    powerline_cwd_prompt "$local_npm_root" "$local_npm_prompt" "$local_git_root"
    powerline_last_status_prompt LAST_STATUS

    LEFT_PROMPT="${SCM_PROMPT}${VIRTUALENV_PROMPT}${RVM_PROMPT}${CWD_PROMPT}${MOVE_CURSOR_RIGHTMOST}"

    ## right prompt ##
    LAST_THEME_COLOR="-"

    powerline_shell_prompt
    powerline_npm_version_prompt "$local_npm_prompt"
    powerline_clock_prompt

    [[ "${SEGMENT_AT_RIGHT}" -eq 1 ]] && (( RIGHT_PROMPT_LENGTH-=1 ))

    RIGHT_PROMPT="\033[${RIGHT_PROMPT_LENGTH}D$(set_rgb_color ${LAST_THEME_COLOR} -)${THEME_PROMPT_LEFT_SEPARATOR}${normal}"
    RIGHT_PROMPT+="${CLOCK_PROMPT}${NPM_VERSION_PROMPT}${SHELL_PROMPT}${normal}"

    PS1="${LEFT_PROMPT}${RIGHT_PROMPT}\n${LAST_STATUS_PROMPT}${PROMPT_CHAR} "
}

PROMPT_COMMAND=powerline_prompt_command
