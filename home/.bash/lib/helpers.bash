# Helper function loading various enable-able files
function _load_bash_it_files() {
  subdirectory="$1"
  if [ ! -d "${BASH_IT}/${subdirectory}" ]
  then
    continue
  fi
  FILES="${BASH_IT}/${subdirectory}/*.bash"
  for bash_config_file in $FILES
  do
    if [ -e "${bash_config_file}" ]; then
      source $bash_config_file
    fi
  done
}

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

# Image width
function wh() {
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
# cd to the path of the front Finder window
cdf() {
    target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`
    if [ "$target" != "" ]; then
        cd "$target"; pwd
    else
        echo 'No Finder window found' >&2
    fi
}

function preexec_invoke_cmd () {
    precmd
    preexec_interactive_mode="yes"
}

# Notes:
#  - tsort requires as input a stream of pairs (a, b) where package a depends
#    on package b. If package a has k > 1 dependencies, we should have k lines
#    associated to it; if package a has no dependencies, then we should have a
#    single line (a, a). The pairs are just space delimited, no parentheses.
#    the little awk program below formats the data that way for tsort.
#  - tsort outputs the order from bottom to top; that's why we need to reverse
#    it with tail -r.
#
# try So I'll try "uninstall... install" instead of "reinstal".
function brew_reinstall () {
    brew list \
        | while read l; do echo -n "$l "; echo $(brew deps $l); done \
        | awk 'NF == 1 {print $1, $1} NF > 1 {for (i=1;i<=NF;i++) print $1, $i}' \
        | tsort \
        | tail -r \
        | while read l; do echo -n "$l "; brew reinstall $l; done
}

# Max length of PWD to display
MAX_PWD_LENGTH=24

# Displays last X characters of pwd
function limited_pwd() {

    # Replace $HOME with ~ if possible
    RELATIVE_PWD=${PWD/#$HOME/\~}

    local offset=$((${#RELATIVE_PWD}-$MAX_PWD_LENGTH))

    if [ $offset -gt "0" ]
    then
        local truncated_symbol="…"
        TRUNCATED_PWD=${RELATIVE_PWD:$offset:$MAX_PWD_LENGTH}
        echo -e "${truncated_symbol}/${TRUNCATED_PWD#*/}"
    else
        echo -e "${RELATIVE_PWD}"
    fi
}

function geticon() {
  APP=`echo $1|sed -e 's/\.app$//'`
  APPDIR=''
  for dir in "/Applications/" "/Applications/Utilities/"; do
    if [[ -d ${dir}$APP.app ]]; then
        APPDIR=$dir
        break
    fi
  done
  if [[ $APPDIR == '' ]]; then
    echo "App not found"
  else
    ICON=`defaults read "${APPDIR}$APP.app/Contents/Info" CFBundleIconFile|sed -e 's/\.icns$//'`
    OUTFILE="$HOME/Downloads/${APP}_icon.jpg"
    MAXAVAIL=`sips -g pixelWidth "${APPDIR}$APP.app/Contents/Resources/$ICON.icns"|tail -1|awk '{print $2}'`
    echo -n "Enter max pixel width ($MAXAVAIL): "
  	read MAX
  	if [[ $MAX == ''  || $MAX -gt $MAXAVAIL ]]; then
  	  MAX=$MAXAVAIL
  	fi
    /usr/bin/sips -s format jpeg --resampleHeightWidthMax $MAX "${APPDIR}$APP.app/Contents/Resources/$ICON.icns" --out "$OUTFILE" > /dev/null 2>&1
    echo "Wrote JPEG to $OUTFILE."
  	echo -n 'Open in Preview? (y/N): '
  	read ANSWER
  	if [[ $ANSWER == 'y' ]]; then
  	  open -a "Preview.app" "$OUTFILE"
  	fi
  fi
}

# http://stackoverflow.com/questions/1378274
# A bad-arse SysOps guy once taught me the Three-Fingered Claw technique:
yell() { echo "$0: $*" >&2; }
die() { yell "$*"; exit 111; }
try() { "$@" || die "cannot $*"; }
asuser() { sudo su - "$1" -c "${*:2}"; }

# Example usage:
# try apt-fast upgrade -y
# try asuser vagrant "echo 'uname -a' >> ~/.profile"

if ! type pathmunge > /dev/null 2>&1
then
  function pathmunge () {
    about 'prevent duplicate directories in you PATH variable'
    group 'lib helpers'
    example 'pathmunge /path/to/dir is equivalent to PATH=/path/to/dir:$PATH'
    example 'pathmunge /path/to/dir after is equivalent to PATH=$PATH:/path/to/dir'

    if ! [[ $PATH =~ (^|:)$1($|:) ]] ; then
      if [ "$2" = "after" ] ; then
        export PATH=$PATH:$1
      else
        export PATH=$1:$PATH
      fi
    fi
  }
fi
