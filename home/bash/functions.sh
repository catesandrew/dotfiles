#######################################################################
# ~/.bash/functions.sh                                                #
# version 0.2.1                                                       #
# by Paul Duncan <pabs@pablotron.org>                                 #
#######################################################################

# Start an HTTP server from a directory, optionally specifying the port
function server() {
    # Get port (if specified)
    local port="${1:-8000}"

    # Open in the browser
    open "http://localhost:${port}/"
    #python -m SimpleHTTPServer "$port"

    # Redefining the default content-type to text/plain instead of the default
    # application/octet-stream allows "unknown" files to be viewable in-browser
    # as text instead of being downloaded.
    #
    # Unfortunately, "python -m SimpleHTTPServer" doesn't allow you to redefine
    # the default content-type, but the SimpleHTTPServer module can be executed
    # manually with just a few lines of code.
    python -c $'import SimpleHTTPServer;\nSimpleHTTPServer.SimpleHTTPRequestHandler.extensions_map[""] = "text/plain";\nSimpleHTTPServer.test();' "$port"
}

function mvim2() {
    mvim --servername `git rev-parse --show-toplevel` .
}

function growl() { echo -e $'\e]9;'${1}'\007' ; return  ; }

# posfind: search the directory frontmost in the Finder
function posfind { find "`/usr/local/bin/posd`" -name "*$1*"; }

# posgrep: grep the directory frontmost in the Finder
function posgrep { grep -iIrn "$1" "`/usr/local/bin/posd`"; }

function tom {
	if [ "`ps xwww | grep -v grep | grep -c catalina`" == "0" ];then
		echo "Off";
	else
		CATALINA_PID=`ps A | grep -v grep | grep catalina | awk '{ print $1 }' | sed 's/[ \t]*$//'`;
		echo "On - $CATALINA_PID";
	fi
}

function killtom {
	if [ "`ps xwww | grep -v grep | grep -c catalina`" == "1" ]; then
		CATALINA_PID=`ps A | grep -v grep | grep catalina | awk '{ print $1 }' | sed 's/[ \t]*$//'`;
	  kill -9 $CATALINA_PID
		echo "Tom is dead. Killed process $CATALINA_PID"
	else
		echo "Tom is not on."
	fi
}

function brainyquote {
  curl --silent http://www.brainyquote.com | egrep '(span class="body")|(span class="bodybold")' | sed -n '6p; 7p; ' | sed ' s/<[a-z0-9=."/ ]*>//g'
}

function quoteoftheday {
  echo `curl --silent http://www.quotedb.com/quote/quote.php?action=quote_of_the_day_rss | awk 'NR==23' | sed -e 's/<[^>]*>//g'`
}

# Image width
function width () {
  echo $(sips -g pixelWidth $1 | grep -oE "[[:digit:]]{1,}$")
}

# Image height
function height () {
  echo $(sips -g pixelHeight $1 | grep -oE "[[:digit:]]{1,}$")
}

# Image width
function wh () {
  width=`identify -format "%[fx:w]" "$1"`;
  height=`identify -format "%[fx:h]" "$1"`;
  echo "width x height = $width x $height"

}

function diffall() {
    for name in $(git diff --name-only $1); do git difftool $1 $name & done
}

# Recursively delete files that match a certain pattern
# (by default delete all `.DS_Store` files)
function cleanup() {
    local q="${1:-*.DS_Store}"
    find . -type f -name "$q" -ls -delete
}

# Create a data URI from a file and copy it to the pasteboard
function datauri() {
    local mimeType=$(file -b --mime-type "$1")
    if [[ $mimeType == text/* ]]; then
        mimeType="${mimeType};charset=utf-8"
    fi
    printf "data:${mimeType};base64,$(openssl base64 -in "$1" | tr -d '\n')" | pbcopy | printf "=> data URI copied to pasteboard.\n"
}

# Compare original and gzipped file size
function gz() {
    local origsize=$(wc -c < "$1")
    local gzipsize=$(gzip -c "$1" | wc -c)
    local ratio=$(echo "$gzipsize * 100 / $origsize" | bc -l)

    printf "orig: %d bytes\n" "$origsize"
    printf "gzip: %d bytes (%2.2f%%)\n" "$gzipsize" "$ratio"
}

decode64 () {
    echo $1 | base64 --decode ; echo
}


# http://www.cyberciti.biz/faq/linux-unix-colored-man-pages-with-less-command/
man() {
    env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man "$@"
}

# http://www.cyberciti.biz/faq/linux-unix-colored-man-pages-with-less-command/
cdf() {
    target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`
    if [ "$target" != "" ]; then
        cd "$target"; pwd
    else
        echo 'No Finder window found' >&2
    fi
}

##################################################
# Fancy PWD display function
##################################################
# The home directory (HOME) is replaced with a ~
# The last pwdmaxlen characters of the PWD are displayed
# Leading partial directory names are striped off
# /home/me/stuff          -> ~/stuff               if USER=me
# /usr/share/big_dir_name -> ../share/big_dir_name if pwdmaxlen=20
# http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x783.html
##################################################
bash_prompt_command() {
    # How many characters of the $PWD should be kept
    local pwdmaxlen=32
    # Indicate that there has been dir truncation
    local trunc_symbol=".."
    local dir=${PWD##*/}
    pwdmaxlen=$(( ( pwdmaxlen < ${#dir} ) ? ${#dir} : pwdmaxlen ))
    NEW_PWD=${PWD/#$HOME/\~}
    local pwdoffset=$(( ${#NEW_PWD} - pwdmaxlen ))
    if [ ${pwdoffset} -gt "0" ]
    then
        NEW_PWD=${NEW_PWD:$pwdoffset:$pwdmaxlen}
        NEW_PWD=${trunc_symbol}/${NEW_PWD#*/}
    fi
}
