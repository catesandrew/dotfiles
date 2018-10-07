# Some aliases for Homebrew

__brew_func_wrap () {
  local cur words cword prev
  _get_comp_words_by_ref -n =: cur words cword prev
  $1
}

__brew_complete () {
  local wrapper="__brew_wrap${2}"
  eval "$wrapper () { __brew_func_wrap $2 ; }"
  complete -o bashdefault -o default -F $wrapper $1 2>/dev/null \
    || complete -o default -F $wrapper $1
}

__brew_c() {
  brew "$@"
}

_completion_loader brew

b() {
    __brew_c "$@"
}
# complete -o bashdefault -o default -F _brew b
__brew_complete b _brew

alias bup='brew update && brew upgrade'
alias bupc='brew update && brew upgrade && brew cleanup'

# TODO: update to check if cask is installed (Linux)
alias bup='brew update && brew upgrade && brew cleanup --prune=1'

bout() {
    __brew_c outdated "$@"
}
__brew_complete bout _brew_outdated

bi() {
    __brew_c install "$@"
}
__brew_complete bi _brew_install

brm() {
    __brew_c uninstall "$@"
}
__brew_complete brm _brew_uninstall

bls() {
    __brew_c list "$@"
}
__brew_complete bls _brew_list

bs() {
    __brew_c search "$@"
}
__brew_complete bs _brew_search

bn() {
    __brew_c info "$@"
}
__brew_complete bn _brew_info

bdr() {
    __brew_c doctor "$@"
}
__brew_complete bdr _brew_doctor
