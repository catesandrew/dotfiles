#!/bin/bash

SHELL_SSH_CHAR=${SHELL_SSH_CHAR:=" "}
SHELL_THEME_PROMPT_COLOR=32
SHELL_THEME_PROMPT_COLOR_SUDO=202

SCM_NONE_CHAR=""
SCM_GIT_CHAR=${POWERLINE_SCM_GIT_CHAR:=" "}
TRUNCATED_SYMBOL="…"

SCM_THEME_PROMPT_CLEAN=""
SCM_THEME_PROMPT_DIRTY=""

SCM_THEME_PROMPT_CLEAN_COLOR=25
SCM_THEME_PROMPT_DIRTY_COLOR=88
SCM_THEME_PROMPT_STAGED_COLOR=30
SCM_THEME_PROMPT_UNSTAGED_COLOR=92
SCM_THEME_PROMPT_COLOR=${SCM_THEME_PROMPT_CLEAN_COLOR}

CWD_THEME_PROMPT_COLOR=240

LAST_STATUS_THEME_PROMPT_COLOR=196

function oh_my_git_scm_prompt {
  # SCM_THEME_PROMPT_DIRTY=' ✗'
  # SCM_THEME_PROMPT_CLEAN=' ✓'
  # SCM_THEME_PROMPT_PREFIX=' |'
  # SCM_THEME_PROMPT_SUFFIX='|'
  # SCM_THEME_BRANCH_PREFIX=''
  # SCM_THEME_TAG_PREFIX='tag:'
  # SCM_THEME_DETACHED_PREFIX='detached:'
  # SCM_THEME_BRANCH_TRACK_PREFIX=' → '
  # SCM_THEME_BRANCH_GONE_PREFIX=' ⇢ '
  # SCM_GIT_CHAR='±'
  # SCM_GIT_DETACHED_CHAR='⌿'
  # SCM_GIT_AHEAD_CHAR="↑"
  # SCM_GIT_BEHIND_CHAR="↓"
  # SCM_GIT_UNTRACKED_CHAR="?:"
  # SCM_GIT_UNSTAGED_CHAR="U:"
  # SCM_GIT_STAGED_CHAR="S:"

  local omg_is_a_git_repo_symbol='±'
  local omg_has_untracked_files_symbol='?:'
  local omg_has_adds_symbol='↑'
  local omg_has_deletions_symbol='↓'
  local omg_has_cached_deletions_symbol=''
  local omg_has_modifications_symbol='✗'
  local omg_has_cached_modifications_symbol='✓'
  local omg_ready_to_commit_symbol=''
  local omg_is_on_a_tag_symbol='tag:'
  local omg_detached_symbol='detached:'
  local omg_can_fast_forward_symbol='ff:'
  local omg_has_diverged_symbol=''
  local omg_not_tracked_branch_symbol=''
  local omg_rebase_tracking_branch_symbol=''
  local omg_merge_tracking_branch_symbol=''
  local omg_should_push_symbol=''
  local omg_has_stashes_symbol='s:'
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
          SCM_PROMPT+="(${SCM_CURRENT_BRANCH} ${type_of_upstream} ${upstream//\/$SCM_CURRENT_BRANCH/})"
        fi
      fi
      if [[ ${SCM_IS_ON_A_TAG} ]]; then SCM_PROMPT+="${omg_is_on_a_tag_symbol} ${tag_at_current_commit}"; fi
    fi

    SCM_PROMPT="[${SCM_PROMPT}]"
    LAST_THEME_COLOR=${SCM_THEME_PROMPT_COLOR}
    (( SEGMENT_AT_LEFT += 1 ))
  else
    SCM_PROMPT=""
  fi
}

function oh_my_git_cwd_prompt {
  local short_pwd=''
  local trunc_short_pwd=''
  local working_dir=''
  local sud_sorking_dir=''
  local git_dir=''
  local sub_git_dir=''

  git_dir="$1"
  working_dir=$(pwd)
  sub_working_dir="${working_dir##*/}"

  if [[ ! -z "${git_dir}" ]]; then
    sub_git_dir="${git_dir##*/}"
  fi

  rootable_limited_pwd '' "$working_dir" 24 short_pwd 30 trunc_short_pwd

  if [[ ! -z "${sub_git_dir}" ]]; then
    title "${sub_git_dir}"
  else
    title "${short_pwd}"
  fi

  CWD_PROMPT=" ${trunc_short_pwd} "
  if [[ "${SEGMENT_AT_LEFT}" -gt 0 ]]; then
    CWD_PROMPT=${CWD_PROMPT}
    SEGMENT_AT_LEFT=0
  fi
  LAST_THEME_COLOR=${CWD_THEME_PROMPT_COLOR}
}

function oh_my_git_last_status_prompt {
  if [[ "$1" -eq 0 ]]; then
    LAST_STATUS_PROMPT=""
  else
    LAST_STATUS_PROMPT=" ${LAST_STATUS} "
  fi
}

function oh_my_git_shell_prompt {
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
  SHELL_PROMPT=" ${SHELL_PROMPT} "
  LAST_THEME_COLOR=${SHELL_PROMPT_COLOR}
  (( SEGMENT_AT_RIGHT += 1 ))
}

scm_prompt() {
  CHAR=$(scm_char)
  if [ $CHAR = $SCM_NONE_CHAR ]
  then
    return
  else
    echo "[$(scm_char)$(scm_prompt_info)]"
  fi
}

function oh_my_git_pure_prompt() {
  # ps_host="${bold_blue}\h${normal}";
  # ps_user="${green}\u${normal}";
  # ps_user_mark="${green} $ ${normal}";
  # ps_root="${red}\u${red}";
  # ps_root_mark="${red} # ${normal}"
  # ps_path="${yellow}\w${normal}";

  # # make it work
  # case $(id -u) in
  #   0) PS1="$ps_root@$ps_host$(scm_prompt):$ps_path$ps_root_mark"
  #      ;;
  #   *) PS1="$ps_user@$ps_host$(scm_prompt):$ps_path$ps_user_mark"
  #      ;;
  # esac

  RIGHT_PROMPT_LENGTH=1
  local LAST_STATUS="$?"
  local local_git_root

  ## left prompt ##
  oh_my_git_scm_prompt
  local_git_root="$(find_git_root)"
  oh_my_git_cwd_prompt "$local_git_root"
  oh_my_git_last_status_prompt LAST_STATUS

  LEFT_PROMPT="${SCM_PROMPT}:${CWD_PROMPT}"

  LAST_THEME_COLOR="-"
  oh_my_git_shell_prompt

  PS1="${LEFT_PROMPT}${LAST_STATUS_PROMPT}${green} $ ${normal}"
}

PROMPT_COMMAND=oh_my_git_pure_prompt;
