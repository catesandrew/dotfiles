# Some aliases for npm

# alias nup='for package in $(npm -g outdated --parseable --depth=0 | cut -d: -f2); do IFS='@' read -ra splits <<< "${package}" && if [ "${splits[0]}" != "npm" ]; then npm -g install "$package"; fi done'

nup() { NPM_CONFIG_PREFIX="${NVM_DIR}/versions/node/v${NVM_VERSION}"; for package in $(\ls -F "${NPM_CONFIG_PREFIX}/lib/node_modules" | sed -n 's/[^@]$//p' | xargs npm outdated --no-color -pg --depth=0 | awk '{gsub(/\/.*\//,"",$1); print}' | awk -F":" '{print $2}'); do IFS=@ read -ra splits <<< "${package}" && if [ "${splits[0]}" != "npm" ]; then npm install -g "${package}"; fi done }

function npm_migrate() {
  local from_ver repos links
  from_ver="$1"

  # Get old globally linked repos
  links=( $(\ls -F "${NVM_DIR}/versions/node/v${from_ver}/lib/node_modules" | sed -n 's/[@]$//p') )

  # Get old globally installed repos
  if hash gsed 2>/dev/null; then
    repos=( $(\ls -F "${NVM_DIR}/versions/node/v${from_ver}/lib/node_modules" | gsed -n 's/[^@]$//p' | awk '!/^npm$/' | awk '!/^@/') )
  else
    repos=( $(\ls -F "${NVM_DIR}/versions/node/v${from_ver}/lib/node_modules" | sed -n 's/[^@]$//p' | awk '!/^npm$/' | awk '!/^@/') )
  fi;

  # echo ${repos[@]}

  # Reinstall repos
  for i in "${!repos[@]}"; do r=${repos[$i]} && printf "%s\t%s\n" "$i" "$r" && npm install -g "$r"; done

  # Relink globally linked repos
  for i in "${!links[@]}"; do l=${links[$i]} && printf "%s\t%s\n" "$i" "$l" && ln -snf "$(readlink "${NVM_DIR}/versions/node/v${from_ver}/lib/node_modules/$l")" "${NVM_DIR}/versions/node/v${NVM_VERSION}/lib/node_modules/$l"; done
}

__npm_func_wrap () {
  local cur words cword prev
  _get_comp_words_by_ref -n =: cur words cword prev
  $1
}

__npm_complete () {
  local wrapper="__npm_wrap${2}"
  eval "$wrapper () { __npm_func_wrap $2 ; }"
  complete -o bashdefault -o default -o nospace -F $wrapper $1 2>/dev/null \
    || complete -o default -o nospace -F $wrapper $1
}

__npm_c() {
  npm "$@"
}

_completion_loader npm

__npm_clean() {
  if [ -f package.json ]; then
    find . -type d -name "node_modules" -exec /bin/rm -rf {} \;
  fi
}

# npm reinstall
nri() {
  __npm_clean
  if [ -f package.json ]; then
    no_lock=1
    if [ -f package-lock.json ]; then
      find . -type f -name "package-lock.json" -exec /bin/rm {} \;
      no_lock=0
    fi

    __npm_clean

    if [ $no_lock ] && [ -f package-lock.json ]; then
      find . -type f -name "package-lock.json" -exec /bin/rm {} \;
    fi

    find . -type f -name "lerna-debug.log" -exec /bin/rm {} \;
  fi
}

ni() {
  if [ -f package.json ]; then
    no_lock=1
    if [ -f package-lock.json ]; then
      no_lock=0
    fi

    if [ $# -eq 0 ]; then
      __npm_c install
    else
      __npm_c install "$@"
    fi

    if [ $no_lock ] && [ -f package-lock.json ]; then
      rm package-lock.json
    fi
  fi
}
__npm_complete ni npm

nid() { __npm_c install --save-dev "$@"; }
# __npm_complete nid npm

nis() { __npm_c install --save "$@"; }
# __npm_complete nis npm

alias nit='npm install-test'
alias nitd='npm install-test --save-dev'
alias nits='npm install-test --save'
alias nl='npm link'
alias nls='npm list'
alias nlsg='npm list --global'
alias nod='npm outdated'

nn() { __npm_c view "$@"; }
# __npm_complete nn npm

alias np='npm publish'
alias nr='npm run'
alias nrb='npm rebuild'
alias nrc='npm run compile'

nt() { __npm_c test "$@"; }
# __npm_complete nt npm

alias nu='npm uninstall'
alias nud='npm uninstall --save-dev'
alias nud='npm update'
alias nus='npm uninstall --save'

nv() { __npm_c view "$@"; }

ns() { __npm_c start "$@"; }

rnc() {
  if [ -d ios ]; then
    /bin/rm -rf ./ios
  fi

  if [ -d android ]; then
    /bin/rm -rf ./android
  fi

  if hash react-native 2>/dev/null; then
    react-native eject
  fi

  if hash watchman 2>/dev/null; then
    watchman watch-del-all
  fi

  __npm_clean

  /bin/rm -rf $TMPDIR/react-*
}

# __npm_complete nv npm
