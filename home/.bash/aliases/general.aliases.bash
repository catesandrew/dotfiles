cite about-alias
about-alias 'general aliases'

# Reload Library
alias reload='source ~/.bash_profile'

# List directory contents
alias sl=ls

# Detect which ls flavor is in use.
if ls --color > /dev/null 2>&1; then # GNU ls
  alias ls="command ls --color"
else # OS X ls
  alias ls="command ls -G"
fi

alias la='ls -AF'       # Compact view, show hidden
alias ll='ls -al'
alias l='ls -a'
alias l1='ls -1'

# My previous shortcuts
# ref: http://ss64.com/osx/ls.html
# Long form no user group, color
#alias l="ls -oG"

# Order by last modified, long form no user group, color
#alias lt="ls -toG"
# List all except . and ..., color, mark file types, long form no user group, file size
#alias la="ls -AGFoh"
# List all except . and ..., color, mark file types, long form no use group, order by last modified, file size
alias lat="ls -AGFoth"
alias l.='ls -d .*'     #list hidden files
#alias ll='ls -lhrt'     #extra info compared to "l"
alias lld='ls -lUd */'  #list directories

alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'

alias _="sudo"

# Allow aliases to be with sudo
alias sudo="sudo "

if [ $(uname) = "Linux" ]
then
  alias ls="ls --color=auto"
fi
which gshuf &> /dev/null
if [ $? -eq 1 ]
then
  alias shuf=gshuf
fi

alias cls='clear'

alias edit="$EDITOR"
alias pager="$PAGER"

alias q='exit'

alias irc="$IRC_CLIENT"

alias ..='cd ..'         # Go up one directory
alias ...='cd ../..'     # Go up two directories
alias ....='cd ../../..' # Go up three directories
alias -- -='cd -'        # Go back

# Shell History
alias h='history'

# Tree
if [ ! -x "$(which tree 2>/dev/null)" ]
then
  alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"
fi

# Directory
alias	md='mkdir -p'
alias	rd='rmdir'

# The problem with `open .` is that if you're inside any kind of bundle
# (application, document, package, etc.) it will open that instead of Finder.
# I prefer to specify the application, and I'm not sure why people take such
# umbrage with that. It's an alias, you're just typing f either way, and my way
# happens to work in more circumstances. So there.
alias of='open -a Finder ./'

# And if you want to quick look from the command line in bash
alias ql='qlmanage -p'
