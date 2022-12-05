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
          version=$(echo "${item}" | cut -d@ -f2)
          pkg=$(echo "${item}" | cut -d@ -f1)
          major=$(echo "$version" | cut -d. -f1)
          installed_version="$(brew info --json "${pkg}@${major}" | jq -r '(.[] | .versions.stable )')"

          HOMEBREW_NO_INSTALL_UPGRADE=1 \
          HOMEBREW_NO_AUTO_UPDATE=1 \
            brew uninstall \
            --ignore-dependencies \
            "${pkg}@${installed_version}"

          # emacs-plus issues with daemon mode, better color emoji support
          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            brew install \
            --ignore-dependencies \
            "${item}" \
            --with-cacodemon-icon \
            --with-xwidgets \
            --with-mailutils \
            --with-no-frame-refocus \
            --with-imagemagick \
            --with-native-comp
          ;;
        bash)
          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            brew upgrade \
            bash
          ;;
        wget)
          HOMEBREW_NO_INSTALL_UPGRADE=1 \
          HOMEBREW_NO_AUTO_UPDATE=1 \
            brew uninstall \
            --ignore-dependencies "${item}"

          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            brew install \
            --ignore-dependencies \
            --HEAD \
            "${item}"
          ;;
        universal-ctags)
          # Given the lack of activity on the official Exuberant Ctags
          # source, it has been forked and renamed to Universal Ctags
          # and can be found at universal-ctags/ctags.
          HOMEBREW_NO_AUTO_UPDATE=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            brew uninstall \
            --ignore-dependencies \
            universal-ctags/universal-ctags/universal-ctags

          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            brew install \
            --ignore-dependencies \
            --HEAD \
            universal-ctags/universal-ctags/universal-ctags
          ;;
        ruby@*)
          # I use rbenv to manage multiple ruby installations. However, some
          # homebrew formulae depend on ruby installed through homebrew so I
          # decided to use ruby from homebrew and link it up through rbenv.
          mkdir -p "${RBENV_ROOT}/versions"

          # Find the old version of Ruby installed through homebrew
          if [ -f "${RBENV_ROOT}/version" ]; then
            RBENV_VERSION=$(head -1 "${RBENV_ROOT}/version")
          fi

          # Uninstall the old version from rbenv
          yes | rbenv uninstall $RBENV_VERSION
          rbenv rehash

          # Find the new version being installed now
          version=$(echo "${item}" | cut -d@ -f2)
          pkg=$(echo "${item}" | cut -d@ -f1)
          major=$(echo "$version" | cut -d. -f1)
          installed_version="$(brew info --json "${pkg}@${major}" | jq -r '(.[] | .versions.stable )')"

          # Now we uninstall the old version of ruby in homebrew
          HOMEBREW_NO_AUTO_UPDATE=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            brew uninstall \
            --ignore-dependencies \
            "${pkg}@${installed_version}"

          # Now install the new version through the update command
          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            brew install \
            --ignore-dependencies \
            "${item}"

          ln -sfv "$(realpath $(brew --prefix "ruby@${version}"))" "$RBENV_ROOT/versions/${version}"
          rbenv global "${version}" system
          rbenv rehash
          ls -al "${RBENV_ROOT}/versions"
          ;;
        python@*)
          mkdir -p "${PYENV_ROOT}/versions"
          pip3 freeze >| "${TMPDIR}/requirements.txt"

          # https://thecesrom.dev/2021/06/28/how-to-add-python-installed-via-homebrew-to-pyenv-versions/
          if [ -f "${PYENV_ROOT}/version" ]; then
            PYENV_VERSION=$(head -1 "${PYENV_ROOT}/version")
          fi
          yes | pyenv uninstall $PYENV_VERSION
          pyenv rehash

          version=$(echo "${item}" | cut -d@ -f2)
          pkg=$(echo "${item}" | cut -d@ -f1)
          major=$(echo "$version" | cut -d. -f1)
          installed_version="$(brew info --json "${pkg}@${major}" | jq -r '(.[] | .versions.stable )')"

          HOMEBREW_NO_AUTO_UPDATE=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            brew uninstall \
            --ignore-dependencies \
            "${pkg}@${installed_version}"

          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            brew install \
            --ignore-dependencies \
            "${item}"

          ln -sfv "$(realpath $(brew --prefix "python@${version}"))" "$PYENV_ROOT/versions/${version}"
          ln -sfv "$(realpath $(brew --prefix "python@${version}"))/Frameworks/Python.framework/Versions/${version}/include/python${version}" "$(realpath $(brew --prefix "python@${version}"))/include"
          pyenv global "${version}" system

          # idle pip python wheel pydoc python-config
          rm --force "$(realpath $(brew --prefix "python@${PYENV_VERSION}"))/bin/idle"
          rm --force "$(realpath $(brew --prefix "python@${PYENV_VERSION}"))/bin/pip"
          rm --force "$(realpath $(brew --prefix "python@${PYENV_VERSION}"))/bin/python"
          rm --force "$(realpath $(brew --prefix "python@${PYENV_VERSION}"))/bin/wheel"
          rm --force "$(realpath $(brew --prefix "python@${PYENV_VERSION}"))/bin/pydoc"
          rm --force "$(realpath $(brew --prefix "python@${PYENV_VERSION}"))/bin/python-config"

          ln -sfv $(realpath $(brew --prefix "python@${version}"))/bin/idle3 "$(realpath $(brew --prefix "python@${version}"))/bin/idle"
          ln -sfv $(realpath $(brew --prefix "python@${version}"))/bin/pip3 "$(realpath $(brew --prefix "python@${version}"))/bin/pip"
          ln -sfv $(realpath $(brew --prefix "python@${version}"))/bin/python3 "$(realpath $(brew --prefix "python@${version}"))/bin/python"
          ln -sfv $(realpath $(brew --prefix "python@${version}"))/bin/wheel3 "$(realpath $(brew --prefix "python@${version}"))/bin/wheel"
          ln -sfv $(realpath $(brew --prefix "python@${version}"))/bin/wheel3 "$(realpath $(brew --prefix "python@${version}"))/bin/pydoc"
          ln -sfv $(realpath $(brew --prefix "python@${version}"))/bin/wheel3 "$(realpath $(brew --prefix "python@${version}"))/bin/python-config"
          pyenv rehash

          ls -al "${PYENV_ROOT}/versions"
          pip3 install -r "${TMPDIR}/requirements.txt"
          ;;
        *)
          HOMEBREW_NO_AUTO_UPDATE=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            brew uninstall \
            --ignore-dependencies \
            "${item}"

          HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
            HOMEBREW_NO_INSTALL_UPGRADE=1 \
            HOMEBREW_NO_AUTO_UPDATE=1 \
            brew install \
            --ignore-dependencies \
            "${item}"
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
    python3)
      local latest latest_semver python_semver installed_semver installed_version version

      mkdir -p "${PYENV_ROOT}/versions"
      pip3 freeze >| "${TMPDIR}/requirements.txt"

      # https://thecesrom.dev/2021/06/28/how-to-add-python-installed-via-homebrew-to-pyenv-versions/
      if [ -f "${PYENV_ROOT}/version" ]; then
        PYENV_VERSION=$(head -1 "${PYENV_ROOT}/version")
        # 3.10
      fi
      yes | pyenv uninstall $PYENV_VERSION
      pyenv rehash

      installed_semver="$(brew info --json python@3 | jq -r '(.[] | .versions.stable )')"
      # 3.10.8
      installed_version="$(cut -d '.' -f 1,2 <<< "${installed_semver}")"
      # 3.10

      latest="$(brew search python@3 | \grep -E '^python(@.*)?$' | sort -r --version-sort | head -n1)"
      # python@3.11
      latest_semver="$(brew info --json "${latest}" | jq -r '(.[] | .versions.stable )')"
      # 3.11.0

      # Now, which formula is latest? `python` or `python@3.y.z`?
      python_semver="$(brew info --json python | jq -r '(.[] | .versions.stable )')"
      # 3.1.2

      __semver_comp $python_semver $latest_semver
      case $? in
        0)
        # they are the same
        ;;
        1)
          # `python` is greater semver, $1 > $2
          latest_semver=$python_semver
          latest=python
          ;;
        2)
          # `python@3.y` is greater semver, $1 < $2
          ;;
      esac

      version="$(cut -d '.' -f 1,2 <<< "${latest_semver}")"
      # 3.11

      HOMEBREW_NO_AUTO_UPDATE=1 \
        HOMEBREW_NO_INSTALL_UPGRADE=1 \
        brew uninstall \
        --ignore-dependencies \
        "python@${installed_version}"

      HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
        HOMEBREW_NO_INSTALL_UPGRADE=1 \
        HOMEBREW_NO_AUTO_UPDATE=1 \
        brew install \
        --ignore-dependencies \
        "$latest"

      ln -sfv "$(realpath $(brew --prefix "python@${version}"))" "$PYENV_ROOT/versions/${version}"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/Frameworks/Python.framework/Versions/${version}/include/python${version}" "$(realpath $(brew --prefix "python@${version}"))/include"
      pyenv global "${version}" system

      rm --force "$(realpath $(brew --prefix "python@${installed_version}"))/bin/idle"
      rm --force "$(realpath $(brew --prefix "python@${installed_version}"))/bin/pip"
      rm --force "$(realpath $(brew --prefix "python@${installed_version}"))/bin/python"
      rm --force "$(realpath $(brew --prefix "python@${installed_version}"))/bin/wheel"
      rm --force "$(realpath $(brew --prefix "python@${installed_version}"))/bin/pydoc"
      rm --force "$(realpath $(brew --prefix "python@${installed_version}"))/bin/python-config"

      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/idle${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/idle"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/idle${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/idle3"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/pip${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/pip"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/pip${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/pip3"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/python${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/python"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/python${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/python3"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/wheel${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/wheel"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/wheel${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/wheel3"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/wheel${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/pydoc"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/wheel${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/pydoc3"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/wheel${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/python-config"
      ln -sfv "$(realpath $(brew --prefix "python@${version}"))/bin/wheel${version}" "$(realpath $(brew --prefix "python@${version}"))/bin/python3-config"
      pyenv rehash

      ls -al "${PYENV_ROOT}/versions"
      pip3 install -r "${TMPDIR}/requirements.txt"
      ;;
    ruby)
      local latest latest_semver ruby_semver installed_semver installed_version version

      mkdir -p "${RBENV_ROOT}/versions"

      if [ -f "${RBENV_ROOT}/version" ]; then
        installed_version=$(head -1 "${RBENV_ROOT}/version")
        # 3.1
      fi
      if [ -n $installed_version ]; then
        yes | rbenv uninstall $installed_version
        rbenv rehash
      fi

      installed_semver="$(brew info --json ruby | jq -r '(.[] | .versions.stable )')"
      # 3.1.2
      installed_version="$(cut -d '.' -f 1,2 <<< "${installed_semver}")"
      # 3.1

      latest="$(brew search ruby | \grep -E '^ruby(@.*)?$' | sort -r --version-sort | head -n1)"
      # ruby@3.0
      latest_semver="$(brew info --json "${latest}" | jq -r '(.[] | .versions.stable )')"
      # 3.0.4

      # Now, which formula is latest? `ruby` or `ruby@3.y.z`?
      ruby_semver="$(brew info --json ruby | jq -r '(.[] | .versions.stable )')"
      # 3.1.2

      __semver_comp $ruby_semver $latest_semver
      case $? in
        0)
          # they are the same
          ;;
        1)
          # `ruby` is greater semver, $1 > $2
          latest_semver=$ruby_semver
          latest=ruby
          ;;
        2)
          # `ruby@3.y.z` is greater semver, $1 < $2
          ;;
      esac

      version="$(cut -d '.' -f 1,2 <<< "${latest_semver}")"
      # 3.1

      HOMEBREW_NO_AUTO_UPDATE=1 \
        HOMEBREW_NO_INSTALL_UPGRADE=1 \
        brew uninstall \
        --ignore-dependencies \
        "ruby@${installed_version}"

      HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
        HOMEBREW_NO_INSTALL_UPGRADE=1 \
        HOMEBREW_NO_AUTO_UPDATE=1 \
        brew install \
        --ignore-dependencies \
        "$latest"

      ln -sfv "$(realpath $(brew --prefix "ruby@${version}"))" "$RBENV_ROOT/versions/${version}"
      rbenv global "${version}" system
      rbenv rehash

      ls -al "${RBENV_ROOT}/versions"
      ;;
    *)
      HOMEBREW_NO_AUTO_UPDATE=1 \
        HOMEBREW_NO_INSTALL_UPGRADE=1 \
        brew uninstall \
        --ignore-dependencies \
        "${item}"

      HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
        HOMEBREW_NO_INSTALL_UPGRADE=1 \
        HOMEBREW_NO_AUTO_UPDATE=1 \
        brew install \
        --ignore-dependencies \
        "${item}"
  esac
}
__brew_complete bu _brew_install

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
