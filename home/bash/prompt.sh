# Eterm Title #
if [ $TERM = "xterm-256color" ]; then
    XTITLE="\[\e]0;\u@\h (\w) - Bash \v\a\]"
else
    XTITLE=""
fi

# Prompt Colors #
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

# Default Colors #
C_USER="$WHITE"
C_OP_PAREN="$DARK_GRAY"
C_AMP="$NOTHING"
C_HOST="$CYAN"
C_CL_PAREN="$DARK_GRAY"
C_COLON="$NOTHING"
C_PWD="$WHITE"
C_GT="$NOTHING"

# Custom Hostname Colors #
. ~/.bash/host_colors.sh

# Custom Root Prompt Colors #
if [ $EUID = "0" ]; then
    C_OP_PAREN="$YELLOW"
    C_USER="$LIGHT_RED"
    C_CL_PAREN="$YELLOW"
fi

# Assign to PS1 #
PS1="$XTITLE""$C_USER""\u""$C_AMP""@""\
$C_HOST""\h""$C_COLON"":\
$C_PWD""\W""$C_GT"">""$NOTHING "

# Sexy Bash Prompt, inspired by "Extravagant Zsh Prompt"
# Screenshot: http://img.gf3.ca/d54942f474256ec26a49893681c49b5a.png
# A big thanks to \amethyst on Freenode

if tput setaf 1 &> /dev/null; then
    tput sgr0
    if [[ $(tput colors) -ge 256 ]] 2>/dev/null; then
        MAGENTA=$(tput setaf 9)
        ORANGE=$(tput setaf 172)
        GREEN=$(tput setaf 190)
        PURPLE=$(tput setaf 141)
        WHITE=$(tput setaf 256)
    else
        MAGENTA=$(tput setaf 5)
        ORANGE=$(tput setaf 4)
        GREEN=$(tput setaf 2)
        PURPLE=$(tput setaf 1)
        WHITE=$(tput setaf 7)
    fi
    BOLD=$(tput bold)
    RESET=$(tput sgr0)
else
    MAGENTA="\033[1;31m"
    ORANGE="\033[1;33m"
    GREEN="\033[1;32m"
    PURPLE="\033[1;35m"
    WHITE="\033[1;37m"
    BOLD=""
    RESET="\033[m"
fi

parse_git_dirty () {
    [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working directory clean" ]] && echo "*"
}

parse_git_branch () {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(parse_git_dirty)/"
}

bash_prompt() {
    PS1="\[${BOLD}${MAGENTA}\]$XTITLE\u \[$WHITE\]at \[$ORANGE\]\h \[$WHITE\]in \[$GREEN\]\$NEW_PWD\[$WHITE\]\$([[ -n \$(git branch 2> /dev/null) ]] && echo \" on \")\[$PURPLE\]\$(parse_git_branch)\[$WHITE\]\n\$ \[$RESET\]"
}

PROMPT_COMMAND=bash_prompt_command

# Make new shells get the history lines from all previous
# shells instead of the default "last window closed" history
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

bash_prompt
unset bash_prompt
