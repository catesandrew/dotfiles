#######################################################################
# ~/.bash/aliases.sh                                                  #
# version 0.2.1                                                       #
# by Paul Duncan <pabs@pablotron.org>                                 #
#######################################################################

########################
# Basic System Aliases #
########################

#alias ls="BLOCK_SIZE=\'1 ls --color=auto" #enable thousands grouping and colour
#alias units='units -t' #terse mode
#alias diff='LC_ALL=C TZ=GMT0 diff -Naur' #normalise diffs for distribution #normalise diffs for distribution
#alias xterm='xterm -fb "" -bg black -fg gray -fa "Sans Mono" -fs 9 +sb -sl 3000 -g 80x50+1+1'
#alias sudo='sudo env PATH=$PATH' #work around sudo built --with-secure-path (ubuntu)
#alias vim='vim -X' #don't try to contact xserver (which can hang on network issues)
#alias gdb='gdb -tui -quiet' #enable the text window interface if possible
#alias head='head -n $((${LINES:-12}-2))' #as many as possible without scrolling
#alias tail='tail -n $((${LINES:-12}-2)) -s.1' #Likewise, also more responsive -f

alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'

# Allow aliases to be with sudo
alias sudo="sudo "

# misc
alias ..="cd .."        #go to parent dir
alias ...="cd ../.."    #go to grandparent dir
alias ~="cd ~"
alias -- -="cd -"       #go to previous dir

# List dir contents aliases
# ref: http://ss64.com/osx/ls.html
# Long form no user group, color
alias l="ls -oG"
# Order by last modified, long form no user group, color
#alias lt="ls -toG"
# List all except . and ..., color, mark file types, long form no user group, file size
alias la="ls -AGFoh"
# List all except . and ..., color, mark file types, long form no use group, order by last modified, file size
alias lat="ls -AGFoth"

alias l.='ls -d .*'     #list hidden files
alias ll='ls -lhrt'     #extra info compared to "l"
alias lld='ls -lUd */'  #list directories
alias cls="clear"

# Concatenate and print content of files (add line numbers)
alias catn="cat -n"

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en1"

# Copy my public key to the pasteboard
alias pubkey="more ~/.ssh/id_rsa.pub | pbcopy | printf '=> Public key copied to pasteboard.\n'"

# Flush DNS cache
alias flushdns="dscacheutil -flushcache"

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

# Show/hide hidden files in Finder
alias showdotfiles="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
alias hidedotfiles="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"

# Hide/show all desktop icons (useful when presenting)
alias showdeskicons="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"
alias hidedeskicons="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"

# Tell less not to beep and also display colours
#export LESS="-QR"
# Adjust the less highlight colors
#export LESS_TERMCAP_so="$(printf 'rev\nbold\nsetaf 3\n' | tput -S)"
#export LESS_TERMCAP_se="$(tput sgr0)"

# Let me have core dumps
#ulimit -c unlimited
ulimit -n 2048

