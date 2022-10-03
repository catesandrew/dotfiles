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

# alias bup='brew update && brew upgrade && brew cleanup --prune=1'
bup() {
  local outdated_list cask_outdated_list

  brew update
  outdated_list=$(brew outdated | perl -pe 's/, /|/g; tr/()//d' | perl -ane 'printf "%s %s %s\n", $F[0], $F[1], $F[3]')

  if [ -n "$outdated_list" ]; then
    echo "found: $outdated_list"

    for item in $(echo "$outdated_list"); do
      echo "Upgrading '$item'"
      HOMEBREW_NO_AUTO_UPDATE=1 brew uninstall --ignore-dependencies "$item"
      case "$item" in
        hunspell)
          # download dictionaries from http://wordlist.aspell.net/dicts/, insall in ~/Library/Spelling/
          brew install hunspell --ignore-dependencies
          ;;
        emacs-plus)
          # emacs-plus issues with daemon mode, better color emoji support
          brew install emacs-plus --with-cacodemon-icon --with-xwidgets --with-mailutils --with-no-frame-refocus --with-imagemagick --with-native-comp --ignore-dependencies
          ;;
        wget)
          brew install wget --ignore-dependencies --HEAD
          ;;
        universal-ctags)
          # Given the lack of activity on the official Exuberant Ctags
          # source, it has been forked and renamed to Universal Ctags
          # and can be found at universal-ctags/ctags.
          brew install --HEAD --ignore-dependencies universal-ctags/universal-ctags/universal-ctags
          ;;
        *)
          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 HOMEBREW_NO_AUTO_UPDATE=1 brew install --ignore-dependencies "$item"
        esac
    done

    HOMEBREW_NO_INSTALL_CLEANUP=1 brew cleanup --prune=1
  else
    echo "No Updates Found"
  fi

  cask_outdated_list=$(brew outdated --greedy | perl -pe 's/, /|/g; tr/()//d' | perl -ane 'printf "%s %s %s\n", $F[0], $F[1], $F[3]')

  if [ -n "$cask_outdated_list" ]; then
    echo "found: $cask_outdated_list"

    for f in $(echo "$cask_outdated_list"); do
      echo "Reinstalling '$f'"
      HOMEBREW_NO_AUTO_UPDATE=1 brew reinstall --cask "$f"
    done

    HOMEBREW_NO_INSTALL_CLEANUP=1 brew cleanup --prune=1
  else
    echo "No Cask Updates Found"
  fi
}

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
