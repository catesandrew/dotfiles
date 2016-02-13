# Reload Library
alias reload='source ~/.bash_profile'

# List directory contents
alias sl=ls

# Compact view, show hidden
alias la='ls -AF'
# Use a long listing format ##
alias ll='ls -al'
# extra info compared to "l"
#alias ll='ls -lhrt'
alias l='ls -a'
alias l1='ls -1'

# My previous shortcuts
# ref: http://ss64.com/osx/ls.html
# Long form no user group, color
# alias l="ls -oG"

# Order by last modified, long form no user group, color
# alias lt="ls -toG"
# List all except . and ..., color, mark file types, long form no user group, file size
#alias la="ls -AGFoh"
# List all except . and ..., color, mark file types, long form no use group, order by last modified, file size
alias lat="ls -AGFoth"
# Show hidden files
alias l.='ls -d .*'
# list directories
alias lld='ls -lUd */'

alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'

alias edit="$EDITOR"
alias pager="$PAGER"

alias irc="$IRC_CLIENT"

# get rid of command not found
alias cd..='cd ..'

# a quick way to get out of current directory
alias ..='cd ..'
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
# Add safety nets

# do not delete / or prompt if deleting more than 3 files at a time
alias rm='rm -I --preserve-root'

# confirmation
# alias mv='mv -i'
# alias cp='cp -i'
# alias ln='ln -i'

# Parenting changing perms on /
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

alias wget='wget -c'            # Resume wget by default
