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

# about 'checks whether a website is down for you, or everybody'
# param '1: website url'
# example '$ down4me http://www.google.com'
function down4me () {
    curl -s "http://www.downforeveryoneorjustme.com/$1" | sed '/just you/!d;s/<[^>]*>//g'
}

# about 'displays your ip address, as seen by the Internet'
function myip () {
    res=$(curl -s checkip.dyndns.org | grep -Eo '[0-9\.]+')
    echo -e "Your public IP is: ${echo_bold_green} $res ${echo_normal}"
}

# about 'generates random password from dictionary words'
# param 'optional integer length'
# param 'if unset, defaults to 4'
# example '$ pass'
# example '$ pass 6'
function pass () {
    local i pass length=${1:-4}
    pass=$(echo $(for i in $(eval echo "{1..$length}"); do pickfrom /usr/share/dict/words; done))
    echo "With spaces (easier to memorize): $pass"
    echo "Without (use this as the pass): $(echo $pass | tr -d ' ')"
}

# about 'make a directory and cd into it'
# param 'path to create'
# example '$ mkcd foo'
# example '$ mkcd /tmp/img/photos/large'
function mkcd () {
    mkdir -p "$*"
    cd "$*"
}

# about 'search through directory contents with grep'
# group 'base'
function lsgrep () {
  ls | grep "$*"
}

# about 'disk usage per directory, in Mac OS X and Linux'
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

# about 'checks for existence of a command'
# param '1: command to check'
# example '$ command_exists ls && echo exists'
function command_exists () {
    type "$1" &> /dev/null ;
}

# useful for administrators and configs
# about 'back up file with timestamp'
# param 'filename'
function buf () {
    local filename=$1
    local filetime=$(date +%Y%m%d_%H%M%S)
    cp ${filename} ${filename}_${filetime}
}

# http://stackoverflow.com/questions/1378274
# A bad-arse SysOps guy once taught me the Three-Fingered Claw technique:
yell() { echo "$0: $*" >&2; }
die() { yell "$*"; exit 111; }
try() { "$@" || die "cannot $*"; }
asuser() { sudo su - "$1" -c "${*:2}"; }
