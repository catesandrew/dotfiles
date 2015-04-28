#!/bin/bash

PROJECT=SWEP
JIRA_CLI="java -jar /usr/local/lib/jira-cli/lib/jira-cli-3.7.0.jar --server ${JIRA_SERVER} --user ${JIRA_USER} --password ${JIRA_PASS}"
USAGE='[help|title|html2md]'

USAGE='[help|title|html2md] [<moweb-id>]'
LONG_USAGE='jira help
        Print this long help message

title
        Copy jira ticket title to clipboard

html2md
        Export jira ticket into markdown format

cli-help
        Print the long jira cli official help message

add-bug
        Create a bug.

add-task
        Creaet a task.

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

Notes
        All documents which are related to the enhancement/task must be
        referenced or linked to the Jira task. More comprehensive
        enhancements may require documentation in the iBASEt wiki.

        Reported issue has been Fixed/Delivered
            If this is an Enhancement, resolve it as Delivered.
            If this is a bug, resolve it as Fixed.

'

die() {
	echo >&2 "$@"
	exit 1
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

dashless=$(basename "$0" | sed -e 's/-/ /')

usage() {
    die "Usage: $dashless $USAGE"
}

if [ -z "$LONG_USAGE" ]
then
    LONG_USAGE="Usage: $dashless $USAGE"
else
    LONG_USAGE="Usage: $dashless $USAGE

$LONG_USAGE"
fi

case "$1" in
    -h|--h|--he|--hel|--help)
    echo "$LONG_USAGE"
    exit
esac

case "$#" in
    0)
        usage ;;
    *)
        CMD="$1"

        case "$CMD" in
            help)
                jira -h ;;
            title)
                shift
                title_helper "$@"
                ;;
            html2md)
                shift
                html2md_helper "$@"
                ;;
            cli-help)
                echo `${JIRA_CLI} --help`
                ;;
            add-bug)
                RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Bug" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --priority "Medium" --environment "" --components "" --summary "Bug Uncaught TypeError: ..."`
                RESULT=`echo ${RESULT} | sed s/.*https/https/g`
                echo "Bug created: ${RESULT}"
                python -mwebbrowser ${RESULT}
                ;;
            add-task)
                RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Task" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Task ..."`
                RESULT=`echo ${RESULT} | sed s/.*https/https/g`
                echo "Task created: ${RESULT}"
                python -mwebbrowser ${RESULT}
                ;;
            add-master-task)
                RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Task" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Collector for ..."`
                RESULT=`echo ${RESULT} | sed s/.*https/https/g`
                echo "Master Task created: ${RESULT}"
                python -mwebbrowser ${RESULT}
                ;;
            add-enhancement)
                RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Enhancement" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Enhancement ..."`
                RESULT=`echo ${RESULT} | sed s/.*https/https/g`
                echo "Enhancement created: ${RESULT}"
                python -mwebbrowser ${RESULT}
                ;;
            add-master-enhancement)
                RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --type "Enhancement" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Collector for ..."`
                RESULT=`echo ${RESULT} | sed s/.*https/https/g`
                echo "Master Enhancement created: ${RESULT}"
                python -mwebbrowser ${RESULT}
                ;;
            #add-sub-)
                #RESULT=`${JIRA_CLI} --action createIssue --project "${PROJECT}" --parent "${PROJECT}-" --type "Enhancement" --custom 'customfield_10100:11331','customfield_10201:-1','customfield_10200:-1' --assignee "acates" --environment "" --components "" --summary "Collector for ..."`
                #RESULT=`echo ${RESULT} | sed s/.*https/https/g`
                #echo "Master Enhancement created: ${RESULT}"
                #python -mwebbrowser ${RESULT}
                #;;
            delete-issue)
                echo `${JIRA_CLI} --action deleteIssue --issue "${PROJECT}-"`
                ;;
            list-components)
                echo `${JIRA_CLI} --action getComponentList --project "${PROJECT}"`
                ;;
            list-versions)
                echo `${JIRA_CLI} --action getVersionList --project "${PROJECT}"`
                ;;

            *)
                `${JIRA_CLI} "${@}"`
                ;;
        esac
esac
