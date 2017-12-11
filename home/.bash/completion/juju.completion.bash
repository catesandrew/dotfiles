# juju-core.bash_completion.sh: dynamic bash completion for juju 2 cmdline,
# from parsed (and cached) juju status output.
#
# Author: JuanJo Ciarlante <jjo@canonical.com>
# Copyright 2016+, Canonical Ltd.
# License: GPLv3
#
# Includes --model and --controller handling:
#   juju list-models --controller <TAB>
#   juju switch <TAB>
#   juju status --model <TAB>
#   juju ssh --model <TAB> [... will complete with proper model's units/etc ...]
#

# Print (return) cache filename for "juju status"
_JUJU_2_juju_status_cache_fname() {
    local model=$(_get_current_model)
    local juju_status_file=${cache_dir}/juju-status-"${model}"
    _JUJU_2_cache_cmd ${_JUJU_2_cache_TTL} \
      echo ${_juju_cmd_JUJU_2?} status --model "${model}" --format json
    return $?
}
# Print (return) all machines
_JUJU_2_machines_from_status() {
    local cache_fname=$(_JUJU_2_juju_status_cache_fname)
    [ -n "${cache_fname}" ] || return 0
${_juju_cmd_PYTHON?} -c '
import json, sys
sys.stderr.close()
j = json.load(sys.stdin)
print ("\n".join(j.get("machines", {}).keys()));
'   < ${cache_fname}
}

# Print (return) all units, each optionally postfixed by $2 (eg. 'myservice/0:')
_JUJU_2_units_from_status() {
    local cache_fname=$(_JUJU_2_juju_status_cache_fname)
    [ -n "${cache_fname}" ] || return 0
${_juju_cmd_PYTHON?} -c '
trail = "'${2}'"
import json, sys
sys.stderr.close()
j = json.load(sys.stdin)
all_units = []
for k, v in j.get("applications", {}).items():
    all_units.extend(v.get("units", {}).keys())
print ("\n".join([unit + trail for unit in all_units]))
'   < ${cache_fname}
}

# Print (return) all applications
_JUJU_2_applications_from_status() {
    local cache_fname=$(_JUJU_2_juju_status_cache_fname)
    [ -n "${cache_fname}" ] || return 0
    ${_juju_cmd_PYTHON?} -c '
import json, sys
sys.stderr.close()
j = json.load(sys.stdin)
print ("\n".join(j.get("applications", {}).keys()))
'   < ${cache_fname}
}

# Print (return) all actions IDS from (cached) "juju show-action-status" output
_JUJU_2_action_ids_from_action_status() {
    local model=$(_get_current_model)
    local juju_status_file=${cache_dir}/juju-status-"${model}"
    local cache_fname=$(
      _JUJU_2_cache_cmd ${_JUJU_2_cache_TTL} \
        echo ${_juju_cmd_JUJU_2?} show-action-status --model "${model}" --format json
    ) || return $?
    [ -n "${cache_fname}" ] || return 0
    ${_juju_cmd_PYTHON?} -c '
import json, sys
sys.stderr.close()
print ("\n".join([x["id"] for x in json.load(sys.stdin).get("actions", {})]))
'   < ${cache_fname}
}

# Print (return) all storage IDs from (cached) "juju list-storage" output
# Caches "juju list-storage" output, print(return) cache filename
_JUJU_2_storage_ids_from_list_storage() {
    local model=$(_get_current_model)
    local juju_status_file=${cache_dir}/juju-status-"${model}"
    local cache_fname=$(
      _JUJU_2_cache_cmd ${_JUJU_2_cache_TTL} \
        echo ${_juju_cmd_JUJU_2?} list-storage --model "${model}" --format json
    ) || return $?
    [ -n "${cache_fname}" ] || return 0
    ${_juju_cmd_PYTHON?} -c '
import json, sys
sys.stderr.close()
print ("\n".join(json.load(sys.stdin).get("storage", {}).keys()))
'   < ${cache_fname}
}

# Print (return) both applications and units, currently used for juju status completion
_JUJU_2_applications_and_units_from_status() {
    _JUJU_2_applications_from_status
    _JUJU_2_units_from_status
}

# Print (return) both units and machines
_JUJU_2_units_and_machines_from_status() {
    _JUJU_2_units_from_status
    _JUJU_2_machines_from_status
}

# Print (return) all juju commands
_JUJU_2_list_commands() {
    ${_juju_cmd_JUJU_2?} help commands 2>/dev/null | awk '{print $1}'
}

# Print (return) flags for juju action
_JUJU_2_flags_for() {
    [ -n "${1}" ] || return 0
    ${_juju_cmd_JUJU_2?} help ${1} 2>/dev/null |egrep -o --  '(^|-)-[a-z-]+'|sort -u
}

_JUJU_2_list_controllers_from_stdin() {
    sed '1s/^$/{}/' |\
    ${_juju_cmd_PYTHON?} -c '
import json, sys
sys.stderr.close()
print ("\n".join(
  json.load(sys.stdin).get("controllers", {}).keys())
)'
}
_JUJU_2_list_models_from_stdin() {
    sed '1s/^$/{}/' |\
    ${_juju_cmd_PYTHON?} -c '
import json, sys
sys.stderr.close()
print ("\n".join(
  ["'$1'" + m["name"] for m in json.load(sys.stdin).get("models", {})]
))'
}
# List all controllers
_JUJU_2_list_controllers_noflags() {
    _JUJU_2_cache_cmd ${_JUJU_2_cache_TTL} cat \
      ${_juju_cmd_JUJU_2?} list-controllers --format json | \
        _JUJU_2_list_controllers_from_stdin
}
# Print:
# - list of controllers as: <controller>:<current_model>
# - list of models under current controller
_JUJU_2_list_controllers_models_noflags() {
    # derive cur_controller from fully specified current model: CONTROLLER:MODEL
    local cur_controller=$(_get_current_model)
    cur_controller=${cur_controller%%:*}

    # List all controller:models
    local controllers=$(_JUJU_2_list_controllers_noflags 2>/dev/null)
    [ -n "${controllers}" ] || { echo "ERROR: no valid controller found (current: ${cur_controller})" >&2; return 0 ;}
    local controller=
    for controller in ${controllers};do
      _JUJU_2_cache_cmd ${_JUJU_2_cache_TTL} cat \
        ${_juju_cmd_JUJU_2?} list-models --controller ${controller} --format json |\
          _JUJU_2_list_models_from_stdin "${controller}:"
      # early break, specially if user hit Ctrl-C
      [ $? -eq 0 ] || return 1
    done

    # List all models under current controller
    _JUJU_2_cache_cmd ${_JUJU_2_cache_TTL} cat \
      ${_juju_cmd_JUJU_2?} list-models --controller ${cur_controller} --format json |\
        _JUJU_2_list_models_from_stdin
}

# Print (return) guessed completion function for cmd.
# Guessing is done by parsing 1st line of juju help <cmd>,
# see case switch below.
_JUJU_2_completion_func_for_cmd() {
    local action=${1} cword=${2}
    # if cword==1 or action==help, use _JUJU_2_list_commands
    if [ "${cword}" -eq 1 -o "${action}" = help ]; then
        echo _JUJU_2_list_commands
        return 0
    fi
    # normally prev_word is just that ...
    local prev_word=${COMP_WORDS[cword-1]}
    # special case for eg:
    #   juju ssh -m myctrl:<TAB>  => COMP_WORDS[cword] is ':'
    #   juju ssh -m myctrl:f<TAB> => COMP_WORDS[cword-1] is ':'
    [[ ${COMP_WORDS[cword]}   == : ]] && prev_word=${COMP_WORDS[cword-2]}
    [[ ${COMP_WORDS[cword-1]} == : ]] && prev_word=${COMP_WORDS[cword-3]}
    case "${prev_word}" in
        --controller|-c)
            echo _JUJU_2_list_controllers_noflags; return 0;;
        --model|-m)
            echo _JUJU_2_list_controllers_models_noflags; return 0;;
        --application)
            echo _JUJU_2_applications_from_status; return 0;;
        --unit)
            echo _JUJU_2_units_from_status; return 0;;
        --machine)
            echo _JUJU_2_machines_from_status; return 0;;
    esac
    # parse 1st line of juju help <cmd>, to guess the completion function
    # order below is important (more specific matches 1st)
    case $(${_juju_cmd_JUJU_2?} help ${action} 2>/dev/null| head -1) in
        # special case for ssh, scp:
        *bootstrap*)
            echo true ;;  # help ok, existing command, no more expansion
        *juju?ssh*|*juju?scp*)
            echo _JUJU_2_units_and_machines_from_status;;
        *\<unit*)
            echo _JUJU_2_units_from_status;;
        *\<service*)
            echo _JUJU_2_applications_from_status;;
        *\<application*)
            echo _JUJU_2_applications_from_status;;
        *\<machine*)
            echo _JUJU_2_machines_from_status;;
        *\<action*)
            echo _JUJU_2_action_ids_from_action_status;;
        *show-storage*)
            echo _JUJU_2_storage_ids_from_list_storage;;
        *pattern*|*application-or-unit*)
            echo _JUJU_2_applications_and_units_from_status;; # e.g. status
        *\<controller*:*\<model*|*--model*)
            echo _JUJU_2_list_controllers_models_noflags;;
        *\<controller?name*)
            echo _JUJU_2_list_controllers_noflags;;
        ?*)
            echo true ;;  # help ok, existing command, no more expansion
        *)
            echo false;;  # failed, not a command
    esac
}

# Print (return) current model as found in the cmdline --model <...>
# else default from $JUJU_MODEL or $(juju switch)
_get_current_model() {
    set +e
    local model=""
    if [[ ${COMP_LINE} =~ .*(--model|-m)\ ([^ ]+)\ (: [^ ]+\ )?.* ]];then
       model="${BASH_REMATCH[2]}${BASH_REMATCH[3]}"
       model="${model// /}"
    fi
    if [ -z "${model}" ];then
       model=${JUJU_MODEL:-$(${_juju_cmd_JUJU_2?} switch)}
    fi
    echo "$model"
}
# Generic command cache function: caches cmdline output, called as:
# _JUJU_2_cache_cmd TTL ACTION cmd args ...
#   TTL:    cache expiration in mins
#   ACTION: what to do with cached filename:
#           - cat (return content)
#           - echo (return cache filename, think "pointer")
_JUJU_2_cache_cmd() {
    local cache_ttl="${1:?missing TTL}" # TTL in mins
    local ret_action=${2:?missing what to return: "echo" or "cat"}
    shift 2
    local cmd="${*:?}"
    local cache_dir=$HOME/.cache/juju
    local cache_file=${cmd}
    # replace / by _
    cache_file=${cache_file//\//_}
    # replace space by __
    cache_file=${cache_file// /__}
    # under cache_dir
    cache_file=${cache_dir}/${cache_file}
    local cmd_pid=
    test -d ${cache_dir} || install -d ${cache_dir} -m 700
    # older than TTL => remove
    find "${cache_file}" -mmin +${cache_ttl} -a -size +64c -delete 2> /dev/null
    # older than TTL/2 or missing => refresh in background
    local cache_refresh=$((${cache_ttl}/2))
    if [[ -z $(find "${cache_file}" -mmin -${cache_refresh} -a -size +64c 2> /dev/null) ]]; then
        # ... create it in background (locking the .tmp to avoid many runs against same cache file
        coproc flock -xn "${cache_file}".tmp \
          sh -c "$cmd > ${cache_file}.tmp && mv -f ${cache_file}.tmp ${cache_file}; rm -f ${cache_file}.tmp"
    fi
    # if missing => wait
    [ ! -s "${cache_file}" -a -n "${COPROC[0]}" ] && read -u ${COPROC[0]}
    # if still missing => fail
    [ ! -s "${cache_file}" ] && echo "" && return 1
    # use it:
    "${ret_action}" "${cache_file}"
}

# Main completion function wrap:
# calls passed completion function, also adding flags for cmd
_JUJU_2_complete_with_func() {
    local action="${1}" func=${2?}
    # scp is special, as we want ':' appended to unit names,
    # and filename completion also.
    local postfix_str= compgen_xtra=
    if [[ ${action} == scp ]]; then
        postfix_str=':'
        compgen_xtra='-A file'
        compopt -o nospace
    fi

    # build COMPREPLY from passed function stdout, and _JUJU_2_flags_for $action
    # don't clutter with cmd flags for functions named *_noflags
    local flags
    case "${func}" in
        *_noflags) flags="";;
        *) flags=$(_JUJU_2_flags_for "${action}");;
    esac

    #echo "** comp=$(set|egrep ^COMP) ** func=$func **" >&2

    # properly handle ':'
    # see http://stackoverflow.com/questions/10528695/how-to-reset-comp-wordbreaks-without-effecting-other-completion-script
    local cur="${COMP_WORDS[COMP_CWORD]}"
    _get_comp_words_by_ref -n : cur
    COMPREPLY=( $( compgen ${compgen_xtra} -W "$(${func} ${postfix_str}) $flags" -- ${cur} ))
    __ltrim_colon_completions "$cur"

    if [[ ${action} == scp ]]; then
        compopt +o nospace
    fi


    return 0
}

# Not used here, available to the user for quick cache removal
_JUJU_2_rm_completion_cache() {
    rm -fv $HOME/.cache/juju/juju-status-*
}

# main completion function entry point
_juju_complete_2() {
    local action parsing_func
    action="${COMP_WORDS[1]}"
    COMPREPLY=()
    parsing_func=$(_JUJU_2_completion_func_for_cmd "${action}" ${COMP_CWORD})
    test -n "${parsing_func}" && \
      _JUJU_2_complete_with_func "${action}" "${parsing_func}"
    return $?
}
# _JUJU_2_cache_TTL [mins]
export _JUJU_2_cache_TTL=2

# All above completion is juju-2 specific, uses $_juju_cmd_JUJU_2

# Detect juju built from source (highest priority)
if [ -x "$GOPATH/bin/juju" ]; then
    export _juju_cmd_JUJU_2="$GOPATH/bin/juju"
# Detect Homebrew
elif [ -x "/usr/local/bin/juju" ]; then
    export _juju_cmd_JUJU_2="/usr/local/bin/juju"
# Detect installed juju-2 binary (next highest priority)
elif [ -x "/usr/bin/juju-2" ]; then
    export _juju_cmd_JUJU_2="/usr/bin/juju-2"
# Snap version of juju
elif [ -x "/snap/bin/juju" ]; then
    export _juju_cmd_JUJU_2="/snap/bin/juju"
# Use /usr/bin/juju as a last resort.
else
    export _juju_cmd_JUJU_2="/usr/bin/juju"
fi

# Select python3, else python2
export _juju_cmd_PYTHON
for _juju_cmd_PYTHON in /usr/bin/python{3,2};do
  [ -x ${_juju_cmd_PYTHON?} ] && break
done

# Add juju-2 completion
complete -F _juju_complete_2 juju-2

# Also hook "juju" (without version) to make this file "self" sufficient.
#
# Note that in a normal install will be overridden later by
# /etc/bash_completion.d/juju-version which does runtime detection
# of 1.x or 2 autocompletion.
complete -F _juju_complete_2 juju

# vim: ai et sw=2 ts=2
