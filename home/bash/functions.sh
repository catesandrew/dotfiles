#######################################################################
# ~/.bash/functions.sh                                                #
# version 0.2.1                                                       #
# by Paul Duncan <pabs@pablotron.org>                                 #
#######################################################################

# Start an HTTP server from a directory, optionally specifying the port
function server() {
    local port="${1:-8000}"
    open "http://localhost:${port}/"
    python -m SimpleHTTPServer "$port"
}

# moved this to a shell script
# you can get it from http://www.pablotron.org/downloads/google
#function google() { TF=/tmp/wget-"$USER"-"$RANDOM".html; wget -O $TF -q http://www.google.com/search?q=`echo $1|sed 's/ /+/g'`; perl -nle 'if (/href=/i) { $t=""; $u=""; /<A HREF=(.*?)>(.*?)<\/A>/; $t=$2; $u=$1; if ($t && $u && $t !~ /^(<IMG|Cached)/ && $u !~ /^\//) { $c++; $t =~ s/<\/?b>//gi; print "$c. \"$t\":\n    $u"; } }' < $TF; rm $TF; }

function growl() { echo -e $'\e]9;'${1}'\007' ; return  ; }

# cdf: cd to the directory in the Finder's front window
alias cdf='cd "$(/usr/local/bin/posd)"'

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

export LG_BROWSER LG_PREFIX

function brainyquote {
  curl --silent http://www.brainyquote.com | egrep '(span class="body")|(span class="bodybold")' | sed -n '6p; 7p; ' | sed ' s/<[a-z0-9=."/ ]*>//g'
}

function quoteoftheday {
  echo `curl --silent http://www.quotedb.com/quote/quote.php?action=quote_of_the_day_rss | awk 'NR==23' | sed -e 's/<[^>]*>//g'`
  echo `curl --silent http://www.quotedb.com/quote/quote.php?action=quote_of_the_day_rss | awk 'NR==22' | sed -e 's/<[^>]*>//g'`
}

# Image width
width () {
  echo $(sips -g pixelWidth $1 | grep -oE "[[:digit:]]{1,}$")
}

# Image height
height () {
  echo $(sips -g pixelHeight $1 | grep -oE "[[:digit:]]{1,}$")
}

# Image width
wh () {
  width=`identify -format "%[fx:w]" "$1"`;
  height=`identify -format "%[fx:h]" "$1"`;
  echo "width x height = $width x $height"

}

function translate() {
  curl -s "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=`perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$1"`&langpair=`if [ "$3" != "" ]; then echo $2; fi;`%7C`if [ "$3" == "" ]; then echo $2; else echo $3; fi;`" | sed 's/{"responseData": {"translatedText":"\([^"]*\)".*}, .*}/\1\n/'; 
}
function translate2() {
    wget -qO- "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=$1&langpair=$2|${3:-en}" | sed 's/.*"translatedText":"\([^"]*\)".*}/\1\n/';
}

function diffall() {
    for name in $(git diff --name-only $1); do git difftool $1 $name & done
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
