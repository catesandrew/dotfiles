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

__brew_install() {
  HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
    HOMEBREW_NO_INSTALL_UPGRADE=1 \
    HOMEBREW_NO_AUTO_UPDATE=1 \
    __brew_c install \
    --ignore-dependencies \
    "$@"
}

__brew_uninstall() {
  HOMEBREW_NO_AUTO_UPDATE=1 \
    HOMEBREW_NO_INSTALL_UPGRADE=1 \
    __brew_c uninstall \
    --ignore-dependencies \
    "$@"
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
      case "$item" in
        gcc|libgccjit)
          # for emacs-plus native-comp support
          HOMEBREW_NO_INSTALL_UPGRADE=1 \
          HOMEBREW_NO_AUTO_UPDATE=1 \
            brew uninstall \
            --ignore-dependencies \
            "${item}"

          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            brew install \
            --ignore-dependencies \
            --build-from-source \
            "${item}"
        ;;
        d12frosted/emacs-plus/emacs-plus@*)
          source "${BASH_IT}/lib/formula-helpers.bash"
          __formula_emacs
          ;;
        bash)
          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            brew upgrade \
            bash
          ;;
        wget)
          __brew_uninstall "${item}"
          __brew_install --HEAD "${item}"
          ;;
        universal-ctags)
          # Given the lack of activity on the official Exuberant Ctags
          # source, it has been forked and renamed to Universal Ctags
          # and can be found at universal-ctags/ctags.
          __brew_uninstall universal-ctags/universal-ctags/universal-ctags
          __brew_install --HEAD universal-ctags/universal-ctags/universal-ctags
          ;;
        ruby-build)
          __brew_uninstall "${item}"
          __brew_install --HEAD "${item}"
          ;;
        ruby*)
          source "${BASH_IT}/lib/formula-helpers.bash"
          __formula_ruby
          ;;
        python*)
          source "${BASH_IT}/lib/formula-helpers.bash"
          __formula_python
          ;;
        *)
          __brew_uninstall "${item}"
          __brew_install "${item}"
        esac
    done

    HOMEBREW_NO_INSTALL_UPGRADE=1 \
      HOMEBREW_NO_INSTALL_CLEANUP=1 \
      brew cleanup --prune=1
  else
    echo "No Updates Found"
  fi

  cask_outdated_list=$(brew outdated --greedy | perl -pe 's/, /|/g; tr/()//d' | perl -ane 'printf "%s %s %s\n", $F[0], $F[1], $F[3]')

  if [ -n "$cask_outdated_list" ]; then
    echo "found: $cask_outdated_list"

    for f in $(echo "$cask_outdated_list"); do
      echo "Reinstalling '$f'"
      HOMEBREW_NO_INSTALL_UPGRADE=1 \
        HOMEBREW_NO_AUTO_UPDATE=1 \
        brew reinstall --cask "$f"
    done

    HOMEBREW_NO_INSTALL_UPGRADE=1 \
      HOMEBREW_NO_INSTALL_CLEANUP=1 \
      brew cleanup --prune=1
  else
    echo "No Cask Updates Found"
  fi
}

bu() {
  local i line item

  args=("$@")
  for (( i=0; i < ${#args[@]}; i++)); do
    if [[ $i -eq ${#args[@]}-1 ]]; then
      item=${args[$i]}
    else
      line="${line} ${args[$i]}"
    fi
  done

  echo "Upgrading '$item'"
  case "$item" in
    emcacs-plus)
      source "${BASH_IT}/lib/formula-helpers.bash"
      __formula_emacs
      ;;
    python)
      source "${BASH_IT}/lib/formula-helpers.bash"
      __formula_python
      ;;
    ruby)
      source "${BASH_IT}/lib/formula-helpers.bash"
      __formula_ruby
      ;;
    *)
      __brew_uninstall "${item}"
      __brew_install "${item}"
  esac
}
__brew_complete bu _brew_install

bout() {
    __brew_c outdated "$@"
}
__brew_complete bout _brew_outdated

bi() {
  local i line item

  args=("$@")
  for (( i=0; i < ${#args[@]}; i++)); do
    if [[ $i -eq ${#args[@]}-1 ]]; then
      item=${args[$i]}
    else
      line="${line} ${args[$i]}"
    fi
  done

  echo "Installing '$item'"
  case "$item" in
    emcacs-plus)
      source "${BASH_IT}/lib/formula-helpers.bash"
      __formula_emacs
      ;;
    python)
      source "${BASH_IT}/lib/formula-helpers.bash"
      __formula_python
      ;;
    ruby)
      source "${BASH_IT}/lib/formula-helpers.bash"
      __formula_ruby
      ;;
    *)
      __brew_install "${item}"
  esac
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
