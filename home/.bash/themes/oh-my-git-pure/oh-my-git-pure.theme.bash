#!/bin/bash

SHELL_SSH_CHAR=${SHELL_SSH_CHAR:=" "}
SHELL_THEME_PROMPT_COLOR=32
SHELL_THEME_PROMPT_COLOR_SUDO=202

SCM_NONE_CHAR=""
SCM_GIT_CHAR=${POWERLINE_SCM_GIT_CHAR:=" "}

SCM_THEME_PROMPT_CLEAN=""
SCM_THEME_PROMPT_DIRTY=""

SCM_THEME_PROMPT_CLEAN_COLOR=25
SCM_THEME_PROMPT_DIRTY_COLOR=88
SCM_THEME_PROMPT_STAGED_COLOR=30
SCM_THEME_PROMPT_UNSTAGED_COLOR=92
SCM_THEME_PROMPT_COLOR=${SCM_THEME_PROMPT_CLEAN_COLOR}

CWD_THEME_PROMPT_COLOR=240

LAST_STATUS_THEME_PROMPT_COLOR=196

SCM_GIT_PROMPT_STASHED="{bold_blue}⚑ ${reset_color}"
SCM_GIT_PROMPT_UNTRACKED="${cyan}…${reset_color}"
SCM_GIT_PROMPT_CHANGED="{blue}✚${reset_color}"
SCM_GIT_PROMPT_DELETED="${red}-${reset_color}"
SCM_GIT_PROMPT_DELETED_CACHED="${red}-${reset_color}"

function oh_my_git_scm_prompt {
  local omg_has_adds_symbol='↑'
  local omg_has_cached_modifications_symbol='✓'
  local omg_ready_to_commit_symbol=''
  local omg_is_on_a_tag_symbol='tag:'
  local omg_detached_symbol='⌿'
  local omg_can_fast_forward_symbol='ff:'
  local omg_has_diverged_symbol=''
  local omg_not_tracked_branch_symbol=''
  local omg_rebase_tracking_branch_symbol=''
  local omg_merge_tracking_branch_symbol=''
  local omg_should_push_symbol=''
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
      scm_oh_my_git_prompt
      SCM_PROMPT="${SCM_PROMPT_GIT}"
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
  if [[ "$ret_value" -eq 0 ]]; then
    LAST_STATUS_PROMPT=""
  else
    LAST_STATUS_PROMPT=" ${ret_value} "
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

function _oh_my_git_pure_prompt() {
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
  local local_git_root

  ## left prompt ##
  oh_my_git_scm_prompt
  local_git_root="$(find_git_root)"
  oh_my_git_cwd_prompt "$local_git_root"
  oh_my_git_last_status_prompt

  LEFT_PROMPT="${SCM_PROMPT}:${CWD_PROMPT}"

  LAST_THEME_COLOR="-"
  oh_my_git_shell_prompt

  PS1="${LEFT_PROMPT}${LAST_STATUS_PROMPT}${green} $ ${normal}"
}

# PROMPT_COMMAND=_oh_my_git_pure_prompt;
# install_prompt _oh_my_git_pure_prompt
precmd_functions+=(_oh_my_git_pure_prompt)
