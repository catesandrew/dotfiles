#######################################################################
# ~/.bash/prompt.sh                                                   #
# version 0.2.1                                                       #
# by Paul Duncan <pabs@pablotron.org>                                 #
#######################################################################

###############
# Eterm Title #
###############
if [ $TERM = "xterm-256color" ]; then
  XTITLE="\[\e]0;\u@\h (\w) - Bash \v\a\]"
else
  XTITLE=""
fi

#################
# Prompt Colors #
#################
BLACK="\[\033[0;30m\]"
BLUE="\[\033[0;34m\]"
GREEN="\[\033[0;32m\]"
CYAN="\[\033[0;36m\]"
RED="\[\033[0;31m\]"
PURPLE="\[\033[0;35m\]"
BROWN="\[\033[0;33m\]"
LIGHT_GRAY="\[\033[0;37m\]"
DARK_GRAY="\[\033[1;30m\]"
LIGHT_BLUE="\[\033[1;34m\]"
LIGHT_GREEN="\[\033[1;32m\]"
LIGHT_CYAN="\[\033[1;36m\]"
LIGHT_RED="\[\033[1;31m\]"
LIGHT_PURPLE="\[\033[1;35m\]"
YELLOW="\[\033[1;33m\]"
WHITE="\[\033[1;37m\]"
NOTHING="\[\033[0m\]"

##################
# Default Colors #
##################
C_USER="$WHITE"
C_OP_PAREN="$DARK_GRAY"
C_AMP="$NOTHING"
C_HOST="$CYAN"
C_CL_PAREN="$DARK_GRAY"
C_COLON="$NOTHING"
C_PWD="$WHITE"
C_GT="$NOTHING"

##########################
# Custom Hostname Colors #
##########################
. ~/.bash/host_colors.sh

#############################
# Custom Root Prompt Colors #
#############################
if [ $EUID = "0" ]; then
  C_OP_PAREN="$YELLOW"
  C_USER="$LIGHT_RED"
  C_CL_PAREN="$YELLOW"
fi

#################
# Assign to PS1 #
#################
PS1="$XTITLE""$C_USER""\u""$C_AMP""@""\
$C_HOST""\h""$C_COLON"":\
$C_PWD""\W""$C_GT"">""$NOTHING "

# iTerm Tab and Title Customization and prompt customization

# Put the string " [bash]   hostname::/full/directory/path"
# in the title bar using the command sequence
# \[\e]2;[bash]   \h::\]$PWD\[\a\]

# Put the penultimate and current directory 
# in the iterm tab
# \[\e]1;\]$(basename $(dirname $PWD))/\W\[\a\]

# Make a simple command-line prompt:  bash-$

#PS1=$'\[\e]2;\h::\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]'$PS1

bash_prompt() {
  PS1="$XTITLE""$C_USER""\u""$C_AMP""@""$C_HOST""\h""$C_COLON"":""$C_PWD""\$NEW_PWD""$C_GT"">""$NOTHING " 
  #PS1=$'\[\e]2;\h::\]$PWD\[\a\]\[\e]1;\]$(basename "$(dirname "$PWD")")/\W\[\a\]'$PS1   
  #if test -f $git_completion_script; then
    #export PS1='\u:$(__git_ps1 "(%s)") \@:\W> '
    PS1="$XTITLE""$C_USER""\u""$C_COLON"":""\$(__git_ps1 '(%s)')""$C_AMP""@""$C_HOST""\h""$C_COLON"":""$C_PWD""\$NEW_PWD""$C_GT"">""$NOTHING " 
  #fi
  
}

PROMPT_COMMAND=bash_prompt_command
bash_prompt
unset bash_prompt
