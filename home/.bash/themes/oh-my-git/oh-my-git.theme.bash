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

# light bulb 
# repo 
# repo forked 
# repo push 
# repo pull 
# repo force push 
# repo clone 
# cloud download 
# cloud upload 
# tag 
# git pull request 
# git commit 
# git branch 
# git merge 
# git compare 
# issue opened 
# issue reopened 
# issue closed 
# mirror 
# book 
# star 
# comment 
# alert 
# gear 
# tools 
# signout 
# rocket 
# organization 
# unfold 
# fold 
# check 
# arrow up , , , 
# pin 
# graph 
# left 
# clock 
# key 
# diff 
# eye 
# dot 
# square 
# pencil 
# info 
# right 
# link 
# plus 
# three bars 
# code 
# list unordered 
# versions 
# lock 
# diff added 
# diff removed 
# diff modified 
# diff renamed 
# diff ignored 
# milestone 
# history 
# external link 
# x 
# pulse 
# sync 
# home 
# stop 
# bug 
# ellipsis 
# hubot 
# up , , 
# package 
# terminal 
# dash 
# inbox 
# trashcan 
# flame 
# law 
# up 
# down 

SCM_GIT_CHAR=""
SCM_GIT_PROMPT_SYMBOLS_TAG=""
SCM_GIT_PROMPT_SYMBOLS_PREHASH=""
SCM_GIT_PROMPT_SYMBOLS_NO_REMOTE_TRACKING=""
SCM_GIT_PROMPT_SYMBOLS_AHEAD="↑·"
SCM_GIT_PROMPT_SYMBOLS_BEHIND="↓·"

SCM_GIT_PROMPT_STASHED="${cyan}${reset_color} _STASHED_ "
SCM_GIT_PROMPT_UNTRACKED="${purple}${reset_color} _UNTRACKED_ "
SCM_GIT_PROMPT_CHANGED="${blue}${reset_color} _CHANGED_ "
SCM_GIT_PROMPT_CHANGED_CACHED="${bold_blue}${reset_color} _CHANGED_CACHED_ "
SCM_GIT_PROMPT_DELETED="${red}${reset_color} _DELETED_ "
SCM_GIT_PROMPT_DELETED_CACHED="${bold_red}${reset_color} _DELETED_CACHED_ "
SCM_GIT_PROMPT_ADDS="${bold_blue}${reset_color} _ADDS_ "
SCM_GIT_PROMPT_CONFLICTS="${red}${reset_color} _CONFLICTS_ "
SCM_GIT_PROMPT_STAGED="${red}${reset_color} _STAGED_ "
SCM_GIT_PROMPT_TAG="${SCM_GIT_PROMPT_SYMBOLS_TAG} _TAG_"
SCM_GIT_PROMPT_PREHASH="${SCM_GIT_PROMPT_SYMBOLS_PREHASH}  (_PREHASH_)"
SCM_GIT_PROMPT_BRANCH_DIVERGED="${SCM_GIT_PROMPT_SYMBOLS_BEHIND}_BEHIND_  ${SCM_GIT_PROMPT_SYMBOLS_AHEAD}_AHEAD_"
SCM_GIT_PROMPT_BRANCH_FF="${SCM_GIT_PROMPT_SYMBOLS_BEHIND}_BEHIND_  "
SCM_GIT_PROMPT_BRANCH_PUSH="   ${SCM_GIT_PROMPT_SYMBOLS_AHEAD}_AHEAD_"
SCM_GIT_PROMPT_BRANCH_CLEAN=""
SCM_GIT_PROMPT_REBASE=" (_BRANCH_  _UPSTREAM_)"
SCM_GIT_PROMPT_MERGE=" (_BRANCH_  _UPSTREAM_)"
SCM_GIT_PROMPT_NO_REMOTE=" ${SCM_GIT_PROMPT_SYMBOLS_NO_REMOTE_TRACKING}  "
SCM_GIT_PROMPT_NO_UPSTREAM=" (_BRANCH_)"

function oh_my_git_scm_prompt {
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
      scm_oh_my_git_powerline_prompt
      SCM_PROMPT="${SCM_PROMPT_GIT}"
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
  local sub_working_dir=''
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

  local half_len=$((${COLUMNS:-80}/2))  # 40
  local final_max_len=$((half_len))     # 30
  local max_len=$((half_len-6))         # 20
  local npm_prompt_len=${#npm_prompt}
  if [ "$npm_prompt_len" -gt 0 ]; then
    local tenth_len=$((${COLUMNS:-80}/8)) # 10
    local final_max_len=$((${COLUMNS:-80}-(tenth_len*2))) # 60
    local final_max_len=$((final_max_len-npm_prompt_len))
    local max_len=$((final_max_len-6)) # 20
  fi

  if [[ ! -z "${npm_root}" ]]; then
    # strip the package.json filename
    npm_root=$(dirname "${npm_root}")
    rootable_limited_pwd "$npm_root" "$working_dir" $max_len short_pwd $final_max_len trunc_short_pwd
  else
    rootable_limited_pwd '' "$working_dir" $max_len short_pwd $final_max_len trunc_short_pwd
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
    local clock
    clock=" $(date +"${THEME_PROMPT_CLOCK_FORMAT}") "

    CLOCK_PROMPT=$(set_rgb_color - ${CLOCK_THEME_PROMPT_COLOR})${clock}${normal}
    if [[ "${SEGMENT_AT_RIGHT}" -gt 0 ]]; then
      CLOCK_PROMPT+=$(set_rgb_color ${LAST_THEME_COLOR} ${CLOCK_THEME_PROMPT_COLOR})${THEME_PROMPT_LEFT_SEPARATOR}${normal}
      (( RIGHT_PROMPT_LENGTH += SEGMENT_AT_RIGHT - 1 ))
    fi
    RIGHT_PROMPT_LENGTH=$(( ${RIGHT_PROMPT_LENGTH} + ${#clock} ))
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
    oh_my_git_scm_prompt
    local_git_root="$(find_git_root)"

    powerline_cwd_prompt "$local_npm_root" "$local_npm_prompt" "$local_git_root"
    powerline_last_status_prompt LAST_STATUS

    LEFT_PROMPT="${SCM_PROMPT}${CWD_PROMPT}${MOVE_CURSOR_RIGHTMOST}"

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
