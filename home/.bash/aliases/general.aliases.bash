# Reload Library
alias reload='source ~/.bash_profile'

# Compact view, show hidden
alias la='ls -AF'
# Use a long listing format ##
# alias ll='ls -al'
# Converts the symbolic permissions to octal (ie: numbers)
__long_list() {
  ls -al "$@" | sed -e 's/--x/1/g' -e 's/-w-/2/g' -e 's/-wx/3/g' -e 's/r--/4/g' -e 's/r-x/5/g' -e 's/rw-/6/g' -e 's/rwx/7/g' -e 's/---/0/g'
}
alias ll=__long_list
# extra info compared to "l"
#alias ll='ls -lhrt'
# alias l='ls -a'
alias l1='ls -1'

# My previous shortcuts
# ref: http://ss64.com/osx/ls.html
# Long form no user group, color
# alias l="ls -oG"

# List all except . and ..., color, mark file types, long form no user group, file size
#alias la="ls -AGFoh"
# List all except . and ..., color, mark file types, long form no use group, order by last modified, file size
alias lat="ls -AGFoth"
# Show hidden files
alias l.='ls -d .*'
# list directories
alias lld='ls -lUd */'

# There are other switches to GNU ls which are less frequently used, some of
# which turn out to be very useful for programming:
# -S — Sort by filesize.

# List files in order of last modification date, newest first. This is useful
# for very large directories when you want to get a quick list of the most
# recent files changed, maybe piped through head or sed 10q. Probably most
# useful combined with -l. If you want the oldest files, you can add -r to
# reverse the list.
alias lt='ls -tl'
# Order by last modified, long form no user group
alias lt="ls -toG"
# Group files by extension; handy for polyglot code, to group header files and
# source files separately, or to separate source files from directories or build
# files.
alias lx='ls -X'
# Naturally sort version numbers in filenames.
alias lv='ls -v'
# List files recursively. This one is good combined with -l and pipedthrough a
# pager like less.
alias lr='ls -Rl'
alias lxr='ls -XRl'

alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'

alias edit="$EDITOR"
alias pager="$PAGER"

alias irc="$IRC_CLIENT"

# get rid of command not found
alias cd..='cd ..'

# a quick way to get out of current directory
alias ..='cd ..'                # Go up one directory
alias ...='cd ../../../'
# alias ...='cd ../..'            # Go up two directories
alias ....='cd ../../../../'
# alias ....='cd ../../..'        # Go up three directories
alias .....='cd ../../../../'   # Go up four dirs
alias .4='cd ../../../../'      # Go up four dirs
alias .5='cd ../../../../..'    # Go up five dirs
alias -- -='cd -'               # Go back

# Make a directory and cd into it in one command
mcd() { mkdir -p "$1"; cd "$1";}
# cd into a directory and list its contents
cls() { cd "$1"; ls;}

# Shell History
alias h='history'

alias path='echo -e ${PATH//:/\\n}'
alias nowtime='date +"%T"'
alias nowdate='date +"%Y-%m-%d"'

# Tree
if [ ! -x "$(which tree 2>/dev/null)" ]
then
  alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"
fi

# Directory
alias md='mkdir -p'             # Create parent directories on demand
alias mkdir='mkdir -pv'         # Create parent directories on demand
alias rd='rmdir'

# The problem with `open .` is that if you're inside any kind of bundle
# (application, document, package, etc.) it will open that instead of Finder. I
# prefer to specify the application, and I'm not sure why people take such
# umbrage with that. It's an alias, you're just typing f either way, and my way
# happens to work in more circumstances. So there.
alias of='open -a Finder ./'

alias ql='qlmanage -p' # And if you want to quick look from the command line in bash
alias ports='netstat -tulan -p tcp' # Use netstat command to quickly list all
                                    # TCP/UDP port on the server:

# Wake-on-LAN (WOL) is an Ethernet networking standard that allows a server to
# be turned on by a network message. You can quickly wakeup nas devices and
# server using the following aliases:

# alias wakeupnas01='/usr/bin/wakeonlan 00:11:32:11:15:FC'
# alias wakeupnas02='/usr/bin/wakeonlan 00:11:32:11:15:FD'
# alias wakeupnas03='/usr/bin/wakeonlan 00:11:32:11:15:FE'

# Debug web server / cdn problems with curl
alias header='curl -I'             # get web server headers
alias headerc='curl -I --compress' # find out if remote server supports gzip /
                                   # mod_deflate or not #

alias wget='wget -c'            # Resume wget by default

alias ct='column -t'

# For URL encoding, we are going to use a slick method developed by Ruslan Spivak.
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])"'

alias lsusers="cut -d: -f1 /etc/passwd"
