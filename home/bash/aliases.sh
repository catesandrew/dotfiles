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

alias ll='ls -l'
alias la='ls -A'
alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'

# enable directory colors
#eval `dircolors`
#alias ls='ls --color=auto '

# misc
alias ..="cd .."        #go to parent dir
alias ...="cd ../.."    #go to grandparent dir
alias -- -="cd -"       #go to previous dir
alias l.='ls -d .*'     #list hidden files
alias ll='ls -lhrt'     #extra info compared to "l"
alias lld='ls -lUd */'  #list directories
alias cls="clear"

# GREP_COLOR=bright yellow on black bg.
# use GREP_COLOR=7 to highlight whitespace on black terminals
# LANG=C for speed. See also: http://www.pixelbeat.org/scripts/findrepo
#alias grep='GREP_COLOR="1;33;40" LANG=C grep --color=auto'

# I hate noise
set bell-style visible

# Tell less not to beep and also display colours
#export LESS="-QR"
# Adjust the less highlight colors
#export LESS_TERMCAP_so="$(printf 'rev\nbold\nsetaf 3\n' | tput -S)"
#export LESS_TERMCAP_se="$(tput sgr0)"

# Let me have core dumps
#ulimit -c unlimited
ulimit -n 2048

export PERL_LOCAL_LIB_ROOT="/Users/andrew/perl5:$PERL_LOCAL_LIB_ROOT";
export PERL_MB_OPT="--install_base "/Users/andrew/perl5"";
export PERL_MM_OPT="INSTALL_BASE=/Users/andrew/perl5";
export PERL5LIB="/Users/andrew/perl5/lib/perl5:$PERL5LIB";
export PATH="/Users/andrew/perl5/bin:$PATH";

