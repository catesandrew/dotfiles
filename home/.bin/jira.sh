#!/bin/bash

PROJECT=SWEP
JIRA_CLI="java -jar /usr/local/lib/jira-cli/lib/jira-cli-3.7.0.jar --server ${JIRA_SERVER} --user ${JIRA_USER} --password ${JIRA_PASS}"

# CONSTANTS & VARIABLES (Common)

# Project Root Dir
readonly PROJECT_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# Level Colors
readonly LEVEL_COLORS=(39 31 31 33 32 36)

# CONSTANTS & VARIABLES (Project)

# Script version
readonly VERSION=0.0.1

# List of requieed tools, example: REQUIRED_TOOLS=(git ssh)
readonly REQUIRED_TOOLS=(jira)

# Long Options. To expect an argument for an option, just place a : (colon)
# after the proper option flag.
readonly LONG_OPTS=(help version login file: title: sessionid:)

# Short Options. To expect an argument for an option, just place a : (colon)
# after the proper option flag.
readonly SHORT_OPTS=hvlf:t:

# Script name
readonly SCRIPT_NAME=${0##*/}

# CORE

readonly JIRA_URL=${JIRA_URL:-}
declare jira_url="$JIRA_URL"

readonly JIRA_AUTH_URI=/rest/auth/latest/session
declare jira_auth_uri="$JIRA_AUTH_URI"

readonly JIRA_API_URI=/rest/api/latest/
declare jira_api_uri="$JIRA_API_URI"

readonly JIRA_LOGIN=${JIRA_LOGIN:-}
declare jira_login="$JIRA_LOGIN"

readonly JIRA_PASSWORD=${JIRA_PASSWORD:-}
declare jira_password="$JIRA_PASSWORD"

readonly JIRA_SESSION_ID=${JIRA_SESSION_ID:-}
declare jira_session_id="$JIRA_SESSION_ID"

readonly JIRA_AUTH_TYPE=${JIRA_AUTH_TYPE:-}
declare jira_auth_type="$JIRA_AUTH_TYPE"

declare jira_file=

# FUNCTIONS

# Print out messages to STDERR.
function ech() { echo -e "$@" >&2; }

# Print out error messages to STDERR.
function err() { echo -e "\033[0;31mERROR: $@\033[0m" >&2; }

# Shows an error if required tools are not installed.
function required {
  local e=0
  for tool in "$@"; do
    type $tool >/dev/null 2>&1 || {
      e=1 && err "$tool is required for running this script. Please install $tool and try again."
    }
  done
  [[ $e < 1 ]] || exit 2
}

# help command
function help_command() {
  cat <<END;

USAGE:
  $SCRIPT_NAME [options] <command>

OPTIONS:

login   Login Jira-user login. Will be promted if not specified.

COMMANDS:

desc    TRY to get description of the specified ISSUE(es) from jira. For each
        ISSUE in list script will ouput the line in the following format:
        ISSUE_ID — ISSUE_DESC

title
        Copy jira ticket title to clipboard

html2md
        Export jira ticket into markdown format

cli-help
        Print the long jira cli official help message

add-bug
        Create a bug.

add-task
        Create a task.

            Task is the designation for changes required to support an
            Enhancement. Task requires a value for Original Estimate and Time
            Spent before it can be resolved. Information documented in a Task
            usually does not contribute directly to product documentation.

add-enhancement
        Create an enhancement.

            Enhancement is the designation for an issue which describes an
            improvement in product functionality or appearance.

            An Enhancement contains the information used for product
            documentation such as Release Bulletin, User Manuals and
            Configuration Manuals.

            Enhancements which involve multiple sub tasks/enhancements should
            be identified as an Enhancement Collector or Master Task. (Note
            - This may be a way to discern the effort which goes into
            analysis/design from that which goes into development.)

add-master-enhancement
        Create a master enhancement

list-components
        List available components

list-versions
        List available versions

NOTES:
        All documents which are related to the enhancement/task must be
        referenced or linked to the Jira task. More comprehensive
        enhancements may require documentation in the wiki.

        Reported issue has been Fixed/Delivered
            If this is an Enhancement, resolve it as Delivered.
            If this is a bug, resolve it as Fixed.
END
  exit 1
}

function short_help_command() {
  cat <<END;

USAGE:
  $SCRIPT_NAME [-h?] [-l LOGIN] [desc|title|html2md] ISSUE...

END
  exit 1
}

# version command
function version_command() {
  echo "$SCRIPT_NAME version $VERSION"
}

# default command
function default_command() {
  help_command
}

function jira_session_command() {
  jira_session_id="$1"
}

function jira_file_command() {
  jira_file="$1"
}

function jira_login_command() {
  if [[ -z "${jira_login}" ]]; then
	  read -rp "Enter your login for JIRA: " jira_login
    echo ""
  fi

  # getting password for JIRA
  if [[ -z "${jira_password}" ]]; then
    read -rsp "Enter your password for JIRA: " jira_password
    echo ""
  fi

  COOKIE_FILE=cookie.txt
  if [ "${AUTH_TYPE}" = 'cookie' ]; then
	  curl --cookie-jar ${COOKIE_FILE} -H "Content-Type: application/json" -d '{"username":"'${jira_login}'", "password":"'${jira_password}'" }' -X POST ${jira_url}${jira_auth_uri}
  elif [ "${AUTH_TYPE}" = 'oauth' ]; then
    # authentication in JIRA
    jira_session_id=`curl -s -H "Content-Type: application/json" -d "{\"username\":\"${jira_login}\",\"password\":\"${jira_password}\"}" -X POST ${jira_url}${jira_auth_uri} | gsed -r 's/^.+JSESSIONID","value":"([^"]+).+$/\1/ig'`

    if [[ -n $(echo "${jira_session_id}" | grep error) ]]; then
	    err "Wrong login or password!"
    else
      ech "Logged on ${jira_session_id}"
    fi
  fi
}

function desc_command() {
  IFS=' ' read -r -a ISSUES <<< "$1"
  # printf 'ISSUES: %s\n' "${ISSUES[@]}"

  for ((I=0; I<${#ISSUES[@]}; I++)); do
    # if [ "${jira_auth_type}" = 'cookie' ]; then
    # elif [ "${jira_auth_type}" = 'basic' ]; then
    # elif [ "${jira_auth_type}" = 'oauth' ]; then
	    SED=`curl -s -H "Content-Type: application/json" -b JSESSIONID=${jira_session_id} ${jira_url}${jira_api_uri}issue/${ISSUES[$I]}?fields=summary | gsed -n -re 's@\\\["]([^\\\]+)\\\["]@«\1»@ig' -e 's/^.+key":"([^"]+)".+summary":"([^"]+).+$/\1 - \2\n/igp'`
    # fi

	  if [[ -z $SED ]]; then
		  echo "Issue \"${ISSUES[$I]}\" not found or unknown error has occured!"
	  else
		  echo "$SED"
	  fi
  done
}

function attachment_command() {
  IFS=' ' read -r -a ISSUES <<< "$1"

  for ((I=0; I<${#ISSUES[@]}; I++)); do
    if [ "${jira_auth_type}" = 'cookie' ]; then
      result=`curl -D- -b ${COOKIE_FILE} -X POST --header "X-Atlassian-Token: no-check" -F "file=@${jira_file}" ${jira_url}${jira_api_uri}issue/${key}/attachments`
    elif [ "${jira_auth_type}" = 'basic' ]; then
      result=`curl -D- -u ${jira_login}:${jira_password} -X POST --header "X-Atlassian-Token: no-check" -F "file=@${jira_file}" ${jira_url}${jira_api_uri}issue/${key}/attachments`
    elif [ "${jira_auth_type}" = 'oauth' ]; then
	    result=`curl -s -H "Content-Type: application/json" -b JSESSIONID=${jira_session_id} -X POST --header "X-Atlassian-Token: no-check" -F "file=@${jira_file}" ${jira_url}${jira_api_uri}issue/${ISSUES[$I]}/attachments`
    fi

	  if [[ -z $SED ]]; then
		  echo "Issue \"${ISSUES[$I]}\" not found or unknown error has occured!"
	  else
		  echo "$SED"
	  fi
  done
}

function title_helper() {
    local OUT_HTML="jira-${1}.html"

    if [ -z "${JIRA_PASS}" ]; then echo "You need to set your username and password in the script."; exit 1; fi
    wget -q -O .login.html ${JIRA_SERVER}/rest/gadget/1.0/login --post-data="os_username=${JIRA_USER}&os_password=${JIRA_PASS}" --no-check-certificate --save-cookies=.cookies.txt --keep-session-cookies --referer ${JIRA_SERVER}

    wget -q -O - "${JIRA_SERVER}/browse/SWEP-$1" --no-check-certificate --load-cookies=.cookies.txt --keep-session-cookies --referer ${JIRA_SERVER} > "${OUT_HTML}"

    #node.io file_query "${OUT_HTML}" h1#summary-val | pbcopy
    local TITLE=`node.io -s file_query "${OUT_HTML}" h1#summary-val`
    TITLE=`echo ${TITLE} | sed s/\"/\'/g`
    echo "Found: ${1} - \"${TITLE}\""
    echo "[SWEP-${1}](${JIRA_SERVER}/browse/SWEP-${1} \"${TITLE}\")" | pbcopy

    rm .login.html; rm .cookies.txt; rm "${OUT_HTML}"
}

function html2md_helper() {
    if [ -z "${JIRA_PASS}" ]; then echo "You need to set your username and password in the script."; exit 1; fi
    wget -q -O .login.html ${JIRA_SERVER}/rest/gadget/1.0/login --post-data="os_username=${JIRA_USER}&os_password=${JIRA_PASS}" --no-check-certificate --save-cookies=.cookies.txt --keep-session-cookies --referer ${JIRA_SERVER}

    #grep "Image Validation" .login.html >/dev/null && echo "Error: You need to log out/in in a web browser" && exit 1
    wget -q -O - "${JIRA_SERVER}/browse/SWEP-$1" --no-check-certificate --load-cookies=.cookies.txt --keep-session-cookies --referer ${JIRA_SERVER} > out.txt

    #TEXT=`cat out.txt | grep "Generated String:" | cut -f 2 -d : | cut -f 1 -d "<" | tr -d [:blank:]`
    #SHIFT=`cat out.txt | grep "Generated String:" | cut -f 3 -d : | cut -f 1 -d "<" | tr -d [:blank:]`

    #echo "If this doesn't work, you'll need to add this separator:"
    #echo $TEXT

    #for EACH in `echo $TEXT | tr "," "\n" | tr ")" "\n" | tr "#" "\n"| tr "'" "\n"| tr "." "\n"| tr "&" "\n"| tr "(" "\n"| tr "!" "\n"| tr "%" "\n"| tr "$" "\n"| tr "+" "\n"| tr "-" "\n"| tr "/" "\n"| tr "*" "\n"| tr "@" "\n"| tr "^" "\n"| tr "\"" "\n"`
    #do
    #let EACH=$EACH-$SHIFT
    #STRING="$STRING$(printf "\\$(printf "%03o" $EACH)")"
    #done
    #echo The string is $STRING
    #wget -q -O -  http://www.hackthissite.org/missions/prog/11/index.php --post-data="solution=$STRING"  --load-cookies=.cookies.txt --keep-session-cookies --referer http://www.hackthissite.org/missions/prog/11/ > out2.txt
    #grep "answer is wrong" out2.txt | html2text
    #grep -i successfully out2.txt | html2text
    #grep -i "already completed" out2.txt | html2text

    sed -i.bak s_href=\"/_href=\"https://wmobile.atlassian.net/_g out.txt
    sed -i.bak s_src=\"/_src=\"https://wmobile.atlassian.net/_g out.txt
    cat out.txt | html2text -b 0 > $1.md

    rm .login.html; rm .cookies.txt; rm out.txt; rm out.txt.bak
}

function bug_add_helper() {
  RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Bug" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --priority "Medium" --environment "" --components "" --summary "Bug Uncaught TypeError: ..."`
  RESULT=`echo ${RESULT} | sed s/.*https/https/g`
  echo "Bug created: ${RESULT}"
  python -mwebbrowser ${RESULT}
}

function task_add_helper() {
  RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Task" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Task ..."`
  RESULT=`echo ${RESULT} | sed s/.*https/https/g`
  echo "Task created: ${RESULT}"
  python -mwebbrowser ${RESULT}
}

function task_add_master_helper() {
  RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Task" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Collector for ..."`
  RESULT=`echo ${RESULT} | sed s/.*https/https/g`
  echo "Master Task created: ${RESULT}"
  python -mwebbrowser ${RESULT}
}

function enhancement_add_helper() {
  RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Enhancement" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Enhancement ..."`
  RESULT=`echo ${RESULT} | sed s/.*https/https/g`
  echo "Enhancement created: ${RESULT}"
  python -mwebbrowser ${RESULT}
}

function enhancement_add_master_helper() {
  RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Enhancement" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Collector for ..."`
  RESULT=`echo ${RESULT} | sed s/.*https/https/g`
  echo "Master Enhancement created: ${RESULT}"
  python -mwebbrowser ${RESULT}
}

# MAIN
function main() {
  # Required tools
  required $REQUIRED_TOOLS

  # Parse options
  while [[ $# -ge $OPTIND ]] && eval opt=\${$OPTIND} || break
        [[ $opt == -- ]] && shift && break
        if [[ $opt == --?* ]]; then
          opt=${opt#--}; shift

          # Argument to option ?
          OPTARG=;local has_arg=0
          [[ $opt == *=* ]] && OPTARG=${opt#*=} && opt=${opt%=$OPTARG} && has_arg=1

          # Check if known option and if it has an argument if it must:
          local state=0
          for option in "${LONG_OPTS[@]}"; do
            [[ "$option" == "$opt" ]] && state=1 && break
            [[ "${option%:}" == "$opt" ]] && state=2 && break
          done
          # Param not found
          [[ $state = 0 ]] && OPTARG=$opt && opt='?'
          # Param with no args, has args
          [[ $state = 1 && $has_arg = 1 ]] && OPTARG=$opt && opt=::
          # Param with args, has no args
          if [[ $state = 2 && $has_arg = 0 ]]; then
            [[ $# -ge $OPTIND ]] && eval OPTARG=\${$OPTIND} && shift || { OPTARG=$opt; opt=:; }
          fi

          # for the while
          true
        else
          getopts ":$SHORT_OPTS" opt
        fi

  do
    case "$opt" in
      # List of options
      v|version)    version_command; exit 0; ;;
      h|help)       help_command ;;
      l|login)      jira_login_command ;;
      f|file)       jira_file_command "$OPTARG" ;;
      sessionid)    jira_session_command "$OPTARG" ;;
      # Errors
      ::)	err "Unexpected argument to option '$OPTARG'"; exit 2; ;;
      :)	err "Missing argument to option '$OPTARG'"; exit 2; ;;
      \?)	err "Unknown option '$OPTARG'"; exit 2; ;;
      *)	err "Internal script error, unmatched option '$opt'"; exit 2; ;;
    esac
  done
  shift $((OPTIND-1))

  # No more arguments -> call default command
  [[ -z "$1" ]] && default_command

  # Set command and arguments
  command="$1" && shift
  args="$@"

  # Execute the command
  case "$command" in
    # help
    help)                   help_command ;;
    version)                version_command ;;
    desc)                   desc_command "$args" ;;
    title)                  title_helper "$args" ;;
    html2md)                html2md_helper "$args" ;;
    add-task)               task_add_helper "$args" ;;
    add-bug)                bug_add_helper "$args" ;;
    add-master-task)        task_add_master_helper "$args" ;;
    add-enhancement)        enhancement_add_helper "$args" ;;
    add-master-enhancement) enhancement_add_master_helper "$args" ;;
    add-attachment)         attachment_command "$args" ;;

    # Unknown command
    *)        err "Unknown command '$command'"; exit 2; ;;
  esac
}

main "$@"
