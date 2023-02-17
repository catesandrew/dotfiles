#!/bin/bash

# https://stackoverflow.com/questions/4023830/how-to-compare-two-strings-in-dot-separated-version-format-in-bash
__semver_comp () {
  if [[ $1 == $2 ]]
  then
    return 0
  fi
  local IFS=.
  local i ver1=($1) ver2=($2)
  # fill empty fields in ver1 with zeros
  for ((i=${#ver1[@]}; i<${#ver2[@]}; i++))
  do
    ver1[i]=0
  done
  for ((i=0; i<${#ver1[@]}; i++))
  do
    if [[ -z ${ver2[i]} ]]
    then
      # fill empty fields in ver2 with zeros
      ver2[i]=0
    fi
    if ((10#${ver1[i]} > 10#${ver2[i]}))
    then
      return 1
    fi
    if ((10#${ver1[i]} < 10#${ver2[i]}))
    then
      return 2
    fi
  done
  return 0
}

__formula_ruby() {
  local latest latest_semver ruby_semver installed_semver installed_version version
  # I use rbenv to manage multiple ruby installations. However, some
  # homebrew formulae depend on ruby installed through homebrew so I
  # decided to use ruby from homebrew and link it up through rbenv.
  mkdir -p "${RBENV_ROOT}/versions"

  # Find the old version of Ruby installed through homebrew
  if [ -f "${RBENV_ROOT}/version" ]; then
    installed_version=$(head -1 "${RBENV_ROOT}/version")
    # 3.1
  fi
  if [ -n $installed_version ]; then
    # Uninstall the old version from rbenv
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

  # Now we uninstall the old version of ruby in homebrew
  HOMEBREW_NO_AUTO_UPDATE=1 \
    HOMEBREW_NO_INSTALL_UPGRADE=1 \
    brew uninstall \
    --ignore-dependencies \
    "ruby@${installed_version}"

  # Now install the new version through the update command
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
}

__formula_python() {
  local latest latest_semver python_semver installed_semver installed_version version

  mkdir -p "${PYENV_ROOT}/versions"
  pip3 freeze >| "${TMPDIR}/requirements.txt"

  # https://thecesrom.dev/2021/06/28/how-to-add-python-installed-via-homebrew-to-pyenv-versions/
  if [ -f "${PYENV_ROOT}/version" ]; then
    PYENV_VERSION=$(head -1 "${PYENV_ROOT}/version")
  fi
  yes | pyenv uninstall $PYENV_VERSION
  pyenv rehash

  latest_versioned_formulae="$(brew info --json python@3 | jq -r '(.[] | .versioned_formulae | first )')"
  # python@3.10
  latest_stable_semver="$(brew info --json python@3 | jq -r '(.[] | .versions.stable)')"
  # 3.11.1
  installed_semver="$(brew info --json "$latest_versioned_formulae" | jq -r '(.[] | .versions.stable )')"
  # 3.10.10

  __semver_comp $latest_stable_semver $installed_semver # '3.11.2' vs '3.10.10'
  case $? in
    0)
    # they are the same
    ;;
    1)
      # 'latest_stable_semver' is greater semver, e.g., $1 > $2
      installed_semver=$latest_stable_semver # now `3.11.2`
      ;;
    2)
      # 'installed_semver' is greater semver, e.g., $1 < $2
      ;;
  esac

  installed_version="$(cut -d '.' -f 1,2 <<< "${installed_semver}")"
  # 3.11

  latest="$(brew search python@3 | \grep -E '^python(@.*)?$' | sort -r --version-sort | head -n1)"
  # python@3.11
  latest_semver="$(brew info --json "${latest}" | jq -r '(.[] | .versions.stable )')"
  # 3.11.2

  # Now, which formula is latest? `python` or `python@3.y.z`?
  python_semver="$(brew info --json python | jq -r '(.[] | .versions.stable )')"
  # 3.11.2

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

  # idle pip python wheel pydoc python-config
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
}

__formula_emacs() {
  local latest latest_semver emacs_semver installed_semver installed_version version

  installed_semver="$(brew info --json emacs-plus@29 | jq -r '(.[] | .versions.stable )')"
  # 29.0.60
  installed_version="$(cut -d '.' -f1 <<< "${installed_semver}")"
  # 29

  # latest="$(brew search emacs-plus@29 | \grep -E 'emacs-plus(@.*)?$' | sort -r --version-sort | head -n1)"
  latest="$(brew search emacs-plus@29 | \grep -E "emacs-plus@${installed_version}$" | sort -r --version-sort | head -n1)"
  # d12frosted/emacs-plus/emacs-plus@29
  latest_semver="$(brew info --json "${latest}" | jq -r '(.[] | .versions.stable )')"
  # 29.0.60

  # Now, which formula is latest? `emacs-plus` or `emacs-plus@3.y.z`?
  emacs_semver="$(brew info --json emacs-plus | jq -r '(.[] | .versions.stable )')"
  # 28.2

  __semver_comp $emacs_semver $latest_semver
  case $? in
    0)
    # they are the same
    ;;
    1)
      # `emacs-plus` is greater semver, $1 > $2
      latest_semver=$emacs_semver
      latest=emacs-plus
      ;;
    2)
      # `emacs-plus@29` is greater semver, $1 < $2
      ;;
  esac

  version="$(cut -d '.' -f1 <<< "${latest_semver}")"
  # 29

  HOMEBREW_NO_INSTALL_UPGRADE=1 \
    HOMEBREW_NO_AUTO_UPDATE=1 \
    brew uninstall \
    --ignore-dependencies \
    "emacs-plus@${installed_version}"

  # emacs-plus issues with daemon mode, better color emoji support
  HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
    HOMEBREW_NO_INSTALL_UPGRADE=1 \
    HOMEBREW_NO_AUTO_UPDATE=1 \
    brew install \
    --ignore-dependencies \
    "${latest}" \
    --with-spacemacs-icon \
    --with-xwidgets \
    --with-mailutils \
    --with-no-frame-refocus \
    --with-imagemagick \
    --with-native-comp
}
