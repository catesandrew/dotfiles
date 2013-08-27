#!/bin/bash
USAGE='[help|title|html2md]'

USAGE='[help|title|html2md] [<moweb-id>]'
LONG_USAGE='jira help
        Print this long help message
jira title
        Copy jira ticket title to clipboard
jira html2md
        Export jira ticket into markdown format
jira cli-help
        Print the long jira cli official help message
'

die() {
	echo >&2 "$@"
	exit 1
}

function title_helper() {
    local OUT_HTML="jira-${1}.html"

    if [ -z "${JIRA_PASS}" ]; then echo "You need to set your username and password in the script."; exit 1; fi
    wget -q -O .login.html https://wmobile.atlassian.net/rest/gadget/1.0/login --post-data="os_username=${JIRA_USER}&os_password=${JIRA_PASS}" --no-check-certificate --save-cookies=.cookies.txt --keep-session-cookies --referer https://wmobile.atlassian.net/

    wget -q -O - "https://wmobile.atlassian.net/browse/MOWEB-$1" --no-check-certificate --load-cookies=.cookies.txt --keep-session-cookies --referer https://wmobile.atlassian.net/ > "${OUT_HTML}"

    #node.io file_query "${OUT_HTML}" h1#summary-val | pbcopy
    local TITLE=`node.io file_query "${OUT_HTML}" h1#summary-val`
    TITLE=`echo ${TITLE} | sed s/\"/\'/g`
    #echo "\"${TITLE} - ${1}\"" | pbcopy
    echo "[MOWEB-${1}](https://wmobile.atlassian.net/browse/MOWEB-${1} \"${TITLE}\")" | pbcopy

    rm .login.html; rm .cookies.txt; rm "${OUT_HTML}"
}

function html2md_helper() {
    if [ -z "${JIRA_PASS}" ]; then echo "You need to set your username and password in the script."; exit 1; fi
    wget -q -O .login.html https://wmobile.atlassian.net/rest/gadget/1.0/login --post-data="os_username=${JIRA_USER}&os_password=${JIRA_PASS}" --no-check-certificate --save-cookies=.cookies.txt --keep-session-cookies --referer https://wmobile.atlassian.net/

    #grep "Image Validation" .login.html >/dev/null && echo "Error: You need to log out/in in a web browser" && exit 1
    wget -q -O - "https://wmobile.atlassian.net/browse/MOWEB-$1" --no-check-certificate --load-cookies=.cookies.txt --keep-session-cookies --referer https://wmobile.atlassian.net/ > out.txt

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
                java -jar /usr/local/lib/jira-cli/lib/jira-cli-3.0.0.jar --help
                ;;
            create-moweb-bug)
                jira --action createIssue --project "Mobile Web" --type "Bug" --fixVersions "Lobster" --affectsVersions "Lobster" --environment "" --components "" --summary "Trying from command line"
                ;;
            create-moweb-task)
                jira --action createIssue --project "Mobile Web" --type "Task" --summary "Trying from command line"
                ;;
            create-sub-story)
                jira --action createIssue --parent "MOWEB-2417" --project "Mobile Web" --type "Story Task" --fixVersions "Lobster" --affectsVersions "Lobster" --components "" --labels "" --summary "Summary: " --description "Description: "
                ;;
            create-serv-bug)
                jira --action createIssue --project "Services & Infrastructure" --type "Bug" --fixVersions "" --affectsVersions "Backlog" --labels "MOWEB" --components "" --summary "Trying from command line"
                ;;
            create-serv-task)
                jira --action createIssue --project "Services & Infrastructure" --type "Task" --summary "Trying from command line"
                ;;
            *)
                java -jar /usr/local/lib/jira-cli/lib/jira-cli-3.0.0.jar --server ${JIRA_SERVER} --user ${JIRA_USER} --password ${JIRA_PASS} "${@}" 
                ;;
        esac
esac

