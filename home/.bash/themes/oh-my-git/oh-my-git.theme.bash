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

omg_is_a_git_repo_symbol=''
omg_has_untracked_files_symbol=''        #                ?    
omg_has_adds_symbol=''
omg_has_deletions_symbol=''
omg_has_cached_deletions_symbol=''
omg_has_modifications_symbol=''
omg_has_cached_modifications_symbol=''
omg_ready_to_commit_symbol=''            #   →
omg_is_on_a_tag_symbol=''                #   
omg_needs_to_merge_symbol='ᄉ'
omg_detached_symbol=''
omg_can_fast_forward_symbol=''
omg_has_diverged_symbol=''               #   
omg_not_tracked_branch_symbol=''
omg_rebase_tracking_branch_symbol=''     #   
omg_merge_tracking_branch_symbol=''      #  
omg_should_push_symbol=''                #    
omg_has_stashes_symbol=''

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
            SCM_PROMPT="${omg_is_a_git_repo_symbol} "

            if [[ $has_stashes ]]; then SCM_PROMPT+="${omg_has_stashes_symbol} "; fi
            if [[ $has_untracked_files ]]; then SCM_PROMPT+="${omg_has_untracked_files_symbol} "; fi
            if [[ $has_modifications ]]; then SCM_PROMPT+="${omg_has_modifications_symbol} "; fi
            if [[ $has_deletions ]]; then SCM_PROMPT+="${omg_has_deletions_symbol} "; fi

            # ready
            if [[ $has_adds ]]; then SCM_PROMPT+="${omg_has_adds_symbol} "; fi
            if [[ $has_modifications_cached ]]; then SCM_PROMPT+="${omg_has_cached_modifications_symbol} "; fi
            if [[ $has_deletions_cached ]]; then SCM_PROMPT+="${omg_has_cached_deletions_symbol} "; fi

            # next operation
            if [[ $ready_to_commit ]]; then SCM_PROMPT+="${omg_ready_to_commit_symbol} "; fi

            # where
            SCM_PROMPT="$(set_rgb_color - ${CWD_THEME_PROMPT_COLOR})${SCM_PROMPT}  ${normal}$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} -)${normal}$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} ${SCM_THEME_PROMPT_COLOR})${THEME_PROMPT_SEPARATOR} ${normal}$(set_rgb_color - ${SCM_THEME_PROMPT_COLOR})"

            if [[ $detached == true ]]; then
                if [[ $detached ]]; then SCM_PROMPT+="${omg_detached_symbol} "; fi
                if [[ $detached ]]; then SCM_PROMPT+="(${current_commit_hash:0:7}) "; fi
            else
                if [[ $has_upstream == false ]]; then
                  SCM_PROMPT+ ="— ${omg_not_tracked_branch_symbol} — (${current_branch}) "
                else
                    if [[ $will_rebase == true ]]; then
                        local type_of_upstream=$omg_rebase_tracking_branch_symbol
                    else
                        local type_of_upstream=$omg_merge_tracking_branch_symbol
                    fi

                    if [[ $has_diverged == true ]]; then
                        SCM_PROMPT+="-${commits_behind} ${omg_has_diverged_symbol} +${commits_ahead}"
                    else
                        if [[ $commits_behind -gt 0 ]]; then
                            SCM_PROMPT+="-${commits_behind} ${omg_can_fast_forward_symbol} —"
                        fi
                        if [[ $commits_ahead -gt 0 ]]; then
                            SCM_PROMPT+="— ${omg_should_push_symbol} +${commits_ahead}"
                        fi
                        if [[ $commits_ahead == 0 && $commits_behind == 0 ]]; then
                            SCM_PROMPT+=" — — "
                        fi

                    fi
                    SCM_PROMPT+="(${current_branch} ${type_of_upstream} ${upstream//\/$current_branch/}) "
                fi
            fi
            if [[ ${is_on_a_tag} ]]; then SCM_PROMPT+="${omg_is_on_a_tag_symbol} ${tag_at_current_commit} "; fi
        fi

        SCM_PROMPT="$(set_rgb_color - ${SCM_THEME_PROMPT_COLOR})${SCM_PROMPT} ${normal}"
        LAST_THEME_COLOR=${SCM_THEME_PROMPT_COLOR}
        (( SEGMENT_AT_LEFT += 1 ))
    else
        SCM_PROMPT=""
    fi
}

# Modified from http://stackoverflow.com/a/1617048/359287

function npm_limited_pwd() {
  local begin="" # The unshortened beginning of the path.
  local shortbegin="" # The shortened beginning of the path.
  local current="" # The section of the path we're currently working on.
  local INHOME=0
  local end="${2:-$(pwd)}/" # The unmodified rest of the path.
  if [[ ! -z "${local_npm_root}" ]]; then
    local offset=${#local_npm_root}
    if [ $offset -gt "0" ]; then
      end=${end:$offset}
    fi
  fi

  if [[ "$end" =~ "$HOME" ]]; then
    INHOME=1
    end="${end#$HOME}" #strip /home/username from start of string
    begin="$HOME"      #start expansion from the right spot
  fi

  end="${end#/}" # Strip the first /
  local shortenedpath="$end" # The whole path, to check the length.
  local maxlength="${3:-0}"

  shopt -q nullglob && NGV="-s" || NGV="-u" # Store the value for later.
  shopt -s nullglob    # Without this, anything that doesn't exist in the filesystem turns into */*/*/...

  while [[ "$end" ]] && (( ${#shortenedpath} > maxlength ))
  do
    current="${end%%/*}" # everything before the first /
    end="${end#*/}"    # everything after the first /

    local shortcur
    if [[ "${current}" && -z "${end}" ]]; then
      shortcur="$current"
      shortcurstar="$current" # No star if we don't shorten it.
    else
      shortcur="$current"
      shortcurstar="$current" # No star if we don't shorten it.

      for ((i=${#current}-2; i>=0; i--)); do
        subcurrent="${current:0:i}"
        matching=("$begin/$subcurrent"*) # Array of all files that start with $subcurrent.
        (( ${#matching[*]} != 1 )) && break # Stop shortening if more than one file matches.
        shortcur="$subcurrent"
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

  local relative_pwd

  if [ $INHOME -eq 1 ]; then
    relative_pwd="~/$shortenedpath" #make sure it starts with ~/
  else
    relative_pwd="/$shortenedpath" # Make sure it starts with /
  fi
  echo "${relative_pwd}"

  shopt "$NGV" nullglob # Reset nullglob in case this is being used as a function.
}

function powerline_cwd_prompt {
  local_npm_root="$1"
  if [[ ! -z "${npm_root}" ]]; then
   local_npm_root=$(dirname "${local_npm_root}")
   short_pwd=$(npm_limited_pwd)
  else
    short_pwd=$(npm_limited_pwd)
  fi

    CWD_PROMPT="$(set_rgb_color - ${CWD_THEME_PROMPT_COLOR}) ${short_pwd} ${normal}$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} -)${normal}$(set_rgb_color ${CWD_THEME_PROMPT_COLOR} -)${THEME_PROMPT_SEPARATOR}${normal}"
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

function get_npm_root {
  local CURRENT_DIR=$(pwd)

  while [ "$CURRENT_DIR" != "/" ]; do
    if [ -f "$CURRENT_DIR/package.json" ]; then
      echo "$CURRENT_DIR/package.json"
      return
    elif [ -d "$CURRENT_DIR/.git" ]; then
      return
    fi

    CURRENT_DIR=$(dirname $CURRENT_DIR)
  done
}

function powerline_npm_version_prompt {
  # if hash npm 2> /dev/null; then
  #   npm_prompt="$(npm show . version 2> /dev/null)"

  if hash JSON.sh 2> /dev/null; then
    NPM_ROOT="$1"

    if [[ -f "${NPM_ROOT}" ]]; then
      package_json=$(JSON.sh < "${NPM_ROOT}")
      npm_version=$(echo "${package_json}" | grep -e '^\[\"version\"\]' | cut -d " " -f2 | awk -F\" '{print $(NF-1)}')
      npm_name=$(echo "${package_json}" | grep -e '^\[\"name\"\]' | cut -d " " -f2 | awk -F\" '{print $(NF-1)}')
      npm_prompt="${npm_name}@${npm_version}"

      if [[ -z "${npm_prompt}" ]]; then
        NPM_VERSION_PROMPT=""
      else
        NPM_VERSION_PROMPT="$(set_rgb_color - ${NPM_THEME_PROMPT_COLOR}) ${npm_prompt} "
        if [[ "${SEGMENT_AT_RIGHT}" -gt 0 ]]; then
          NPM_VERSION_PROMPT+=$(set_rgb_color ${LAST_THEME_COLOR} ${NPM_THEME_PROMPT_COLOR})${THEME_PROMPT_LEFT_SEPARATOR}${normal}
          (( RIGHT_PROMPT_LENGTH += SEGMENT_AT_RIGHT ))
        fi
        RIGHT_PROMPT_LENGTH=$(( ${RIGHT_PROMPT_LENGTH} + ${#npm_prompt} + 2 ))
        LAST_THEME_COLOR=${NPM_THEME_PROMPT_COLOR}
        (( SEGMENT_AT_RIGHT += 1 ))
      fi
    else
      NPM_VERSION_PROMPT=""
    fi
  else
    NPM_VERSION_PROMPT=""
  fi
}

function powerline_prompt_command() {
    local LAST_STATUS="$?"
    local MOVE_CURSOR_RIGHTMOST='\033[500C'
    local npm_root=$(get_npm_root)
    RIGHT_PROMPT_LENGTH=1

    ## left prompt ##
    powerline_scm_prompt
    powerline_virtualenv_prompt
    powerline_rvm_prompt
    powerline_cwd_prompt "${npm_root}"
    powerline_last_status_prompt LAST_STATUS

    LEFT_PROMPT="${SCM_PROMPT}${VIRTUALENV_PROMPT}${RVM_PROMPT}${CWD_PROMPT}${MOVE_CURSOR_RIGHTMOST}"

    ## right prompt ##
    LAST_THEME_COLOR="-"

    powerline_shell_prompt
    powerline_npm_version_prompt "${npm_root}"
    # powerline_clock_prompt
    CLOCK_PROMPT=""

    [[ "${SEGMENT_AT_RIGHT}" -eq 1 ]] && (( RIGHT_PROMPT_LENGTH-=1 ))

    RIGHT_PROMPT="\033[${RIGHT_PROMPT_LENGTH}D$(set_rgb_color ${LAST_THEME_COLOR} -)${THEME_PROMPT_LEFT_SEPARATOR}${normal}"
    RIGHT_PROMPT+="${CLOCK_PROMPT}${NPM_VERSION_PROMPT}${SHELL_PROMPT}${normal}"

    PS1="${LEFT_PROMPT}${RIGHT_PROMPT}\n${LAST_STATUS_PROMPT}${PROMPT_CHAR} "
}

PROMPT_COMMAND=powerline_prompt_command
