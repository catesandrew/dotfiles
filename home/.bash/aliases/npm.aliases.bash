# Some aliases for npm

# alias nup='for package in $(npm -g outdated --parseable --depth=0 | cut -d: -f2); do IFS='@' read -ra splits <<< "${package}" && if [ "${splits[0]}" != "npm" ]; then npm -g install "$package"; fi done'

nup() { for package in $(\ls -F "${NPM_CONFIG_PREFIX}/lib/node_modules" | sed -n 's/[^@]$//p' | xargs npm outdated --no-color -pg --depth=0 | awk '{gsub(/\/.*\//,"",$1); print}' | \grep -o ":.*:" | sed 's/.$//; s/^.//'); do IFS=@ read -ra splits <<< "${package}" && if [ "${splits[0]}" != "npm" ]; then npm install -g "${package}"; fi done }

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

ni() { __npm_c install "$@"; }
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

nt() { __npm_c test "$@"; }
# __npm_complete nt npm

alias nu='npm uninstall'
alias nud='npm uninstall --save-dev'
alias nud='npm update'
alias nus='npm uninstall --save'

nv() { __npm_c view "$@"; }
# __npm_complete nv npm
