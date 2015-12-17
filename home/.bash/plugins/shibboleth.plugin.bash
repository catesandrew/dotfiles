cite about-plugin
about-plugin 'shibboleth'

# Predictive cd
_shibboleth_cd() {
  ARGS="${@}"
  LAST_ARG="${@: -1}"
  OPTS=""
  if [[ "$ARGS" != "$LAST_ARG" ]]; then
    OPTS="${ARGS% *}"
  fi

  if [[ -z "$LAST_ARG" ]]; then
    # echo "No argument given. Fallback to command cd."
    command cd "$@"
  elif [[ -d "$LAST_ARG" ]]; then
    # echo "Last argument is a directory. Change to it.
    command cd "$@"
  elif [[ -f "$LAST_ARG" ]]; then
    # echo "Last argument is a file. Change to the directory that contains it."
    command cd $OPTS "$(dirname "$LAST_ARG")"
  elif [[ "${LAST_ARG::1}" == "-" ]]; then
    # echo "Last argument is an option. Fallback to command cd."
    command cd "$@"
  elif [[ ! -z "$(fasd -d "$LAST_ARG")" ]]; then
    # echo "Last argument is a directory. Change to it.
    # echo "${green}$(fasd -d "$LAST_ARG")${reset}"
    command cd $OPTS "$(fasd -d "$LAST_ARG")"
  elif [[ ! -z "$(fasd -f "$LAST_ARG")" ]]; then
    # echo "Last argument is a file. Change to the directory that contains it."
    # echo "${green}$(dirname "$(fasd -f "$LAST_ARG")")${reset}"
    command cd $OPTS "$(dirname "$(fasd -f "$LAST_ARG")")"
  else
    # echo "Fallback to command cd."
    command cd "$@"
  fi
}
alias cd='_shibboleth_cd'

# Predictive ls
_shibboleth_ls() {
  ARGS="${@}"
  LAST_ARG="${@: -1}"
  OPTS=""
  if [[ "$ARGS" != "$LAST_ARG" ]]; then
    OPTS="${ARGS% *}"
  fi

  if [[ -z "$LAST_ARG" ]]; then
    # echo "No argument given. List the current directory."
    ls "$@"
  elif [[ -d "$LAST_ARG" ]]; then
    # echo "Last argument is a directory. List it."
    ls "$@"
  elif [[ -f "$LAST_ARG" ]]; then
    # echo "Last argument is a file. List the directory that contains it."
    echo -e "${echo_green}$(dirname "$LAST_ARG")${echo_normal}"
    ls "$(dirname "$LAST_ARG")"
  elif [[ "${LAST_ARG::1}" == "-" ]]; then
    # echo "Last argument is an option. Fallback to command ls."
    ls "$@"
  elif [[ ! -z "$(fasd -d "$LAST_ARG")" ]]; then
    # echo "Last argument is a directory. List it."
    echo -e "${echo_green}$(fasd -d "$LAST_ARG")${echo_normal}"
    ls $OPTS "$(fasd -d "$LAST_ARG")"
  elif [[ ! -z "$(fasd -f "$LAST_ARG")" ]]; then
    # echo "Last argument is a file. List the directory that contains it."
    echo -e "${echo_green}$(dirname "$(fasd -f "$LAST_ARG")")${echo_normal}"
    ls $OPTS "$(dirname "$(fasd -f "$LAST_ARG")")"
  else
    # echo "Fallback to command ls."
    ls "$@"
  fi
}
alias ls='_shibboleth_ls'

# Predictive less
_shibboleth_less() {
  ARGS="${@}"
  LAST_ARG="${@: -1}"
  OPTS=""
  if [[ "$ARGS" != "$LAST_ARG" ]]; then
    OPTS="${ARGS% *}"
  fi

  if [[ -z "$LAST_ARG" ]]; then
    # echo "No argument given. List the current directory."
    echo -e "${echo_green}. is a directory${echo_normal}"
    ls .
  elif [[ -d "$LAST_ARG" ]]; then
    # echo "Last argument is a directory. List it."
    echo -e "${echo_green}$LAST_ARG is a directory${echo_normal}"
    ls "$LAST_ARG"
  elif [[ -f "$LAST_ARG" ]]; then
    # echo "Last argument is a file. Display its contents."
    less "$@"
  elif [[ "${LAST_ARG::1}" == "-" ]]; then
    # echo "Last argument is an option. Fallback to command less."
    less "$@"
  elif [[ ! -z "$(fasd -d "$LAST_ARG")" ]]; then
    # echo "List the specified directory."
    echo -e "${echo_green}$(fasd -d "$LAST_ARG")${echo_normal}"
    ls "$(fasd -d "$LAST_ARG")"
  elif [[ ! -z "$(fasd -f "$LAST_ARG")" ]]; then
    # echo "Last argument is a file. Display its contents."
    echo -e "${echo_green}$(fasd -f "$LAST_ARG")${echo_normal}"
    less $OPTS "$(fasd -f "$LAST_ARG")"
  else
    # echo "Fallback to command less."
    less "$@"
  fi
}
alias less='_shibboleth_less'
alias more='less'

# Teach open about the Internet
_shibboleth_open() {
  ARGS="${@}"
  LAST_ARG="${@: -1}"
  OPTS=""
  if [[ "$ARGS" != "$LAST_ARG" ]]; then
    OPTS="${ARGS% *}"
  fi

  if [[ -z $@ ]]; then
    # echo "No argument given. Fallback to command open."
    open "$@"
  elif [[ -e "$LAST_ARG" ]]; then
    # echo "Last argument is a file or directory. Open it."
    open "$@"
  elif [[ "${LAST_ARG::1}" == "-" ]]; then
    # echo "Last argument is an option. Fallback to command open."
    open "$@"
  elif ([[ ${LAST_ARG,,} == *".co"* ]] ||
        [[ ${LAST_ARG,,} == *".me"* ]] ||
        [[ ${LAST_ARG,,} == *".org"* ]] ||
        [[ ${LAST_ARG,,} == *".net"* ]] ||
        [[ ${LAST_ARG,,} == *".gov"* ]] ||
        [[ ${LAST_ARG,,} == *".io"* ]] ||
        [[ ${LAST_ARG,,} == *"localhost"* ]]) &&
        [[ ! ${LAST_ARG,,} == *"http"* ]]; then
    # echo "Last argument is a website. Open it."
    open $OPTS "http://${LAST_ARG}"
  else
    # echo "Fallback to command open."
    open "$@"
  fi
}
alias open='_shibboleth_open'

verbose() {
  _shibboleth_cd_verbose() {
    _shibboleth_cd "$@" && ls -a
  }
  alias cd='_shibboleth_cd_verbose'
  echo "Maximum verbosity"
}

brief() {
  _shibboleth_cd_brief() {
    _shibboleth_cd "$@" && ls
  }
  alias cd='_shibboleth_cd_brief'
  echo "Medium verbosity"
}

superbrief() {
  alias cd='_shibboleth_cd'
  echo "Low verbosity"
}

splash() {
  echo "But nothing happened."
}

xyzzy() {
  echo "A hollow voice says, 'Fool.'"
}

alias wait="echo \"Time passes.\" && wait"
