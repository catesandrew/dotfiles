# display all ip addresses for this host
function ips () {
    if command -v ifconfig &>/dev/null
    then
        ifconfig | awk '/inet /{ print $2 }'
    elif command -v ip &>/dev/null
    then
        ip addr | grep -oP 'inet \K[\d.]+'
    else
        echo "You don't have ifconfig or ip command installed!"
    fi
}

# checks whether a website is down for you, or everybody
# param '1: website url'
# example '$ down4me http://www.google.com'
function down4me () {
    curl -s "http://www.downforeveryoneorjustme.com/$1" | sed '/just you/!d;s/<[^>]*>//g'
}

# displays your ip address, as seen by the Internet
function myip () {
    res=$(curl -s checkip.dyndns.org | grep -Eo '[0-9\.]+')
    echo -e "Your public IP is: ${echo_bold_green} $res ${echo_normal}"
}

# picks random line from file
# param '1: filename'
# example '$ pickfrom /usr/share/dict/words'
function pickfrom ()
{
    local file=$1
    [ -z "$file" ] && reference $FUNCNAME && return
    length=$(cat $file | wc -l)
    n=$(expr $RANDOM \* $length \/ 32768 + 1)
    head -n $n $file | tail -1
}

# generates random password from dictionary words
# param 'optional integer length'
# param 'if unset, defaults to 4'
# example '$ passgen'
# example '$ passgen 6'
function passgen ()
{
    local i passgen length=${1:-4}
    pass=$(echo $(for i in $(eval echo "{1..$length}"); do pickfrom /usr/share/dict/words; done))
    echo "With spaces (easier to memorize): $pass"
    echo "Without (use this as the password): $(echo $pass | tr -d ' ')"
}

# preview markdown file in a browser
# param '1: markdown file'
# example '$ pmdown README.md'
function pmdown ()
{
    if command -v markdown &>/dev/null
    then
      markdown $1 | browser
    else
      echo "You don't have a markdown command installed!"
    fi
}

# search through directory contents with grep
# group 'base'
function lsgrep () {
  ls | grep "$*"
}

# disk usage per directory, in Mac OS X and Linux
# param '1: directory name'
function disk_usage () {
    if [ $(uname) = "Darwin" ]; then
        if [ -n $1 ]; then
            du -hd $1
        else
            du -hd 1
        fi

    elif [ $(uname) = "Linux" ]; then
        if [ -n $1 ]; then
            du -h --max-depth=1 $1
        else
            du -h --max-depth=1
        fi
    fi
}

# checks for existence of a command
# param '1: command to check'
# example '$ command_exists ls && echo exists'
function command_exists () {
    type "$1" &> /dev/null ;
}

# useful for administrators and configs

# back up file with timestamp
# param 'filename'
function buf () {
    local filename=$1
    local filetime=$(date +%Y%m%d_%H%M%S)
    cp ${filename} ${filename}_${filetime}
}

# move files to hidden folder in tmp, that gets cleared on each reboot
# param 'file or folder to be deleted'
# example 'del ./file.txt'
function del() {
  mkdir -p /tmp/.trash && mv "$@" /tmp/.trash;
}

# http://stackoverflow.com/questions/1378274
# A bad-arse SysOps guy once taught me the Three-Fingered Claw technique:
yell() { echo "$0: $*" >&2; }
die() { yell "$*"; exit 111; }
try() { "$@" || die "cannot $*"; }
asuser() { sudo su - "$1" -c "${*:2}"; }

# Sort the Body of Output While Leaving the Header on the First Line Intact
#
# I find myself wanting to sort the output of commands that contain headers.
# After the sort is performed the header ends up sorted right along with the
# rest of the content. This function will keep the header line intact and allow
# sorting of the remaining lines of output. Here are some examples to illustrate
# the usage of this function:
# command | body sort
# cat file | body sort

body() {
  IFS= read -r header
  printf '%s\n' "$header"
  "$@"
}

# Get an excuse in a single command...
excuse() {
  # echo `telnet bofh.jeffballard.us 666 2>/dev/null` |grep --color -o "Your excuse is:.*$"
  # Created by Ben Okopnik on Mon Apr  1 04:12:39 EST 2002
  line=$(($RANDOM%`grep -c '$' $BASH_IT/excuses`))

  cat -n $BASH_IT/excuses|while read a b
  do
    [ "$a" = "$line" ] && { echo "Your excuse is: $b"; break; }
  done
}

# Use the one liner below to relocate a file or directory, but keep it
# accessible on the old location through a symlink.
lmv() {
  [ -e "$1" -a -d "$2" ] && mv "$1" "$2"/ && ln -s "$2"/"$(basename "$1")" "$(dirname "$1")";
}
