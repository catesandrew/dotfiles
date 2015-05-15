#!/bin/sh

# Set the colours you can use
black='\033[0;30m'
white='\033[0;37m'
red='\033[0;31m'
green='\033[0;32m'
yellow='\033[0;33m'
blue='\033[0;34m'
magenta='\033[0;35m'
cyan='\033[0;36m'


#  Reset text attributes to normal + without clearing screen.
alias Reset="tput sgr0"

# Color-echo.
# arg $1 = message
# arg $2 = Color
cecho() {
  echo "${2}${1}"
  Reset # Reset to normal.
  return
}

print_status() {
  echo
  echo "## $1"
  echo
}

bail() {
  echo 'Error executing command, exiting'
  exit 1
}

exec_cmd_nobail() {
  echo "+ $1"
  bash -c "$1"
}

exec_cmd() {
  exec_cmd_nobail "$1" || bail
}

exec_sudo_cmd_nobail() {
  echo "+ $1"
  sudo bash -c "$1"
}

exec_sudo_cmd() {
  exec_sudo_cmd_nobail "$1" || bail
}

# Set continue to false by default
CONTINUE=false

echo ""
cecho "###############################################" $red
cecho "#        DO NOT RUN THIS SCRIPT BLINDLY       #" $red
cecho "#         YOU'LL PROBABLY REGRET IT...        #" $red
cecho "#                                             #" $red
cecho "#              READ IT THOROUGHLY             #" $red
cecho "#         AND EDIT TO SUIT YOUR NEEDS         #" $red
cecho "###############################################" $red
echo ""

echo ""
cecho "Have you read through the script you're about to run and " $red
cecho "understood that it will make changes to your computer? (y/n)" $red
read -r response
case $response in
  [yY]) CONTINUE=true
      break;;
  *) break;;
esac

if ! $CONTINUE; then
  # Check if we're continuing and output a message if not
  cecho "Please go read the script, it only takes a few minutes" $red
  exit
fi

# Here we go.. ask for the administrator password upfront and run a
# keep-alive to update existing `sudo` time stamp until script has finished
sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

echo ""
echo "Would you like to set your computer name?  (y/n)"
read -r response
case $response in
  [yY])
      echo "What would you like it to be?"
      read COMPUTER_NAME
      sudo hostname $COMPUTER_NAME
      sudo hostnamectl set-hostname $COMPUTER_NAME
      break;;
  *) break;;
esac

PRE_INSTALL_PKGS=""

# Check that HTTPS transport is available to APT
# (Check snaked from: https://get.docker.io/ubuntu/)

if [ ! -e /usr/lib/apt/methods/https ]; then
    PRE_INSTALL_PKGS="${PRE_INSTALL_PKGS} apt-transport-https"
fi

if [ ! -x /usr/bin/lsb_release ]; then
    PRE_INSTALL_PKGS="${PRE_INSTALL_PKGS} lsb-release"
fi

if [ ! -x /usr/bin/curl ] && [ ! -x /usr/bin/wget ]; then
    PRE_INSTALL_PKGS="${PRE_INSTALL_PKGS} curl"
fi

# Populating Cache
print_status "Populating apt-get cache..."
exec_sudo_cmd 'apt-get update'

if [ "X${PRE_INSTALL_PKGS}" != "X" ]; then
    print_status "Installing packages required for setup:${PRE_INSTALL_PKGS}..."
    # This next command needs to be redirected to /dev/null or the script will bork
    # in some environments
    exec_sudo_cmd "apt-get install -y${PRE_INSTALL_PKGS} 2>&1 > /dev/null"
fi

DISTRO=$(lsb_release -c -s)

###############################################################################
# Git
###############################################################################

echo ""
echo "Would you like to install git?  (y/n)"
read -r response
case $response in
  [yY])
      sudo apt-get update
      sudo apt-get install git
      break;;
  *) break;;
esac

###############################################################################
# Linuxbrew
###############################################################################

PREFIX=/usr/local
HOMEBREW_PREFIX="${PREFIX}/linuxbrew"
HOMEBREW_CACHE="${HOMEBREW_PREFIX}/.cache"

echo ""
echo "Install linuxbrew?  (y/n)"
read -r response
case $response in
  [yY])
    # dependencies
    sudo apt-get install build-essential curl git m4 ruby texinfo libbz2-dev libcurl4-openssl-dev libexpat-dev libncurses-dev zlib1g-dev

    sudo chmod g+rwx ${PREFIX}
    sudo chgrp adm ${PREFIX}

    sudo mkdir -p ${HOMEBREW_PREFIX}
    sudo chmod g+rwx ${HOMEBREW_PREFIX}
    # the group is set to wheel by default for some reason
    sudo chgrp adm ${HOMEBREW_PREFIX}

    sudo mkdir -p ${HOMEBREW_CACHE}
    sudo chmod g+rwx ${HOMEBREW_CACHE}

    git clone https://github.com/Homebrew/linuxbrew.git ${HOMEBREW_PREFIX}

    break;;
  *) break;;
esac

echo ""
echo "Would you like to install your dotfiles?  (y/n)"
read -r response
case $response in
  [yY])
      DFM_REPO="${HOME}/.dotfiles"
      git clone --recursive https://github.com/catesandrew/dotfiles.git .dotfiles
      DFM_REPO=.dotfiles .dotfiles/home/.bin/dfm install
      cd "${HOME}/.vim/bundle/vimproc"
      exec_cmd 'make'
      exec_cmd 'cd -'
      exec_cmd 'cp "${DFM_REPO}/home/.gitconfig-local.template ${HOME}/.gitconfig-local"'
      exec_cmd 'cp "${DFM_REPO}/home/.gitconfig-private.template ${HOME}/.gitconfig-private"'

      print_status 'Edit the .gitconfig-local and .gitconfig-private files'

      break;;
  *) break;;
esac

echo ""
echo "Would you like to install powerline fonts?  (y/n)"
read -r response
case $response in
  [yY])
      TMP_DIR=`mktemp -d`
      exec_cmd 'git clone https://github.com/powerline/fonts.git "${TMP_DIR}"'
      exec_cmd 'cd "${TMP_DIR}"'
      exec_cmd './install.sh'
      exec_cmd 'cd -'
      break;;
  *) break;;
esac

echo ""
echo "Would you like to install node via ppa?  (y/n)"
read -r response
case $response in
  [yY])
      curl -sL https://deb.nodesource.com/setup | sudo bash -

      dpkg -s npm &>/dev/null || {
        # curl -sL https://deb.nodesource.com/setup | sudo bash -

        print_status "Updating NodeJS PPA..."

        ## NodeSource's Node.js PPA
        # echo deb https://deb.nodesource.com/node trusty main > /etc/apt/sources.list.d/nodesource.list
        sudo bash -c 'echo "deb https://deb.nodesource.com/node ${DISTRO} main" > /etc/apt/sources.list.d/nodesource.list'
        sudo bash -c 'echo "deb-src https://deb.nodesource.com/node ${DISTRO} main" > /etc/apt/sources.list.d/nodesource.list'
        sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 68576280

        exec_sudo_cmd 'apt-get -yqq update'
        exec_sudo_cmd 'apt-get -y install nodejs'

        print_status "Installing native build tools..."
        apt-get install -y build-essential
      }

      break;;
  *) break;;
esac

echo ""
echo "Would you like to install emacs...?  (y/n)"
read -r response
case $response in
  [yY])
      print_status "Installing build-dep emacs24..."
      exec_sudo_cmd 'apt-get -yqq update'
      exec_sudo_cmd 'apt-get -y build-dep emacs24'

      print_status "Downloading ..."
      TMP_DIR=`mktemp -d`
      exec_cmd 'cd "${TMP_DIR}"'
      exec_cmd 'wget http://mirror.team-cymru.org/gnu/emacs/emacs-24.5.tar.gz'
      exec_cmd 'tar xvf emacs-24.5.tar.gz'
      exec_cmd 'cd emacs-24.5'
      exec_cmd './configure'
      print_status "Making ..."
      exec_cmd 'make'
      print_status "Installing ..."
      exec_sudo_cmd 'make install'
      exec_cmd 'cd --'

      break;;
  *) break;;
esac

echo ""
echo "Would you like to install nvm?  (y/n)"
read -r response
case $response in
  [yY])
      print_status "Installing native build tools..."
      apt-get install -y build-essential libssl-dev
      exec_cmd 'brew install nvm'

      # Load NVM into a shell session *as a function*
      NVM_TARGET="$(brew --prefix nvm)"
      [[ -s "$NVM_TARGET/nvm.sh" ]] && source "$NVM_TARGET/nvm.sh"

      exec_sudo_cmd 'mkdir -p ${PREFIX}/nvm'
      exec_sudo_cmd 'chmod g+rwx ${PREFIX}/nvm'
      exec_sudo_cmd 'chgrp adm ${PREFIX}/nvm'

      exec_cmd 'nvm install -s v0.12.3'
      exec_cmd 'nvm alias default 0.12.3'
      exec_cmd 'nvm use default'

      break;;
  *) break;;
esac

echo ""
echo "Would you like to disable smb password synchronization?  (y/n)"
read -r response
case $response in
  [yY])

      print_status "Launching pam-auth-update ..."
      print_status "Please uncheck SMB password synchronization ..."
      exec_sudo_cmd 'pam-auth-update'

      break;;
  *) break;;
esac

# sudo apt-get -yqq update
# sudo apt-get -yqq upgrade
# sudo apt-get dist-upgrade
# dpkg -l | grep <package-name>
sudo apt-get install lua

sudo apt-get install libtiff5-dev libpng12-dev libjpeg-dev libgif-dev libgnutls-dev libxml2-dev

/usr/bin/setxkbmap -option "ctrl:swapcaps"

brew install editorconfig
brew install cask
cd .emacs.d
cask install

brew install fasd
brew install fzf
/usr/local/linuxbrew/Cellar/fzf/0.9.11/install
git co home/.fzf.bash

# sudo update-rc.d vncserver defaults 99

cd .emacs.d/
sudo apt-get install editorconfig
cask install
