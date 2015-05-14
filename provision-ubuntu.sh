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
      git clone --recursive https://github.com/catesandrew/dotfiles.git .dotfiles
      DFM_REPO=.dotfiles .dotfiles/home/.bin/dfm install
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
        sudo sh -c 'echo "deb https://deb.nodesource.com/node trusty main" > /etc/apt/sources.list.d/nodesource.list'
        sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 68576280

        sudo apt-get -yqq update
        apt-get -y install nodejs

        print_status "Installing native build tools..."
        apt-get install -y build-essential
      }

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

      break;;
  *) break;;
esac

###############################################################################
# General UI/UX
###############################################################################

echo ""
echo "Would you like to set your computer name (as done via System Preferences >> Sharing)?  (y/n)"
read -r response
case $response in
  [yY])
      echo "What would you like it to be?"
      read COMPUTER_NAME
      sudo scutil --set ComputerName $COMPUTER_NAME
      sudo scutil --set HostName $COMPUTER_NAME
      sudo scutil --set LocalHostName $COMPUTER_NAME
      sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string $COMPUTER_NAME
      break;;
  *) break;;
esac

# sudo apt-get -yqq update
# sudo apt-get -yqq upgrade
# sudo apt-get dist-upgrade

dpkg -l | grep vnc
sudo apt-get install lua
brew
which brew
echo $PATH
ln -s ../linuxbrew/bin/brew ./brew
exit
ls .bash
ls
ll
ll .mjolnir/
ll
ls
cat .profile
cd .config
ls
..
ls
ll
cd ..
ll
ls
ll
ls
echo $PATH
which brew
cat .bashrc
ls
z dotfiles
ls
cite
which cite
bash cite
brew
brew doctor
brew search fasd
apt-get search fasd
sudo apt-get install fasd
ll
ls
ll
ls
cat examples.desktop
ls
ll
rm install
ls
ll
sudo apt-get install cite
ls
vim .bashrc
vim .profile
ls .bashrc
vim .profile
ls .bashrc
vim .bashrc
vim .bash_profile
vim .bashrc
ls
ll
ls
ll
ls
ll
ls
cat .bash_profile
mvim .profile
vim .profile
vim .bashrc
cat .profile
cat .bashrc
vim .bash_profile
vi .bash_profile
. .bash_profile
vim .bash/aliases/emacs.aliases.bash
v .bashrc
vi .bashrc
vim .bashrc
vi .profile
vi .bash_profile
vi .profile
ls
cat .bashrc
vi .bashrc
ls
cat .bashrc
vi .bashrc
cat .profile
ll
vim .profile
ls .bash_login
cat .bash_profile
ls
cat .bashrc
cat .profile
cat .bashrc.load
cat .profile
cat .bashrc
cat .bashrc.load
cat .bash_profile
vim .bash_profile
vi .bashrc
vi .bash_profile
vi .bashrc
vi .bash_profile
vi .bashrc
git clone  https://github.com/powerline/fonts.git
cd fonts/
./install.sh
..
rm -rf fonts
sudo apt-get install emacs
sudo apt-get install libtiff5-dev libpng12-dev libjpeg-dev libgif-dev libgnutls-dev libxml2-dev
sudo apt-get install emacs
/usr/bin/setxkbmap -option "ctrl:swapcaps"
vi .lessfilter
emacs
vi .bashrc
ll
ll
brew install cask
cd .emacs.d
cask install
brew install ediorconfig
brew install editorconfig
brew uninstall cask
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
cask install
~/.cask/bin/cask install
emacs
rm -rf ~/.cask
brew install cask
emacs
which cask
vim init.config
vim init.el
vim init.el
vi init.el
emacs
brew install fasd
brew info fasd
vim .bash_profile
vim .bashrc
vim .bashrc.load
brew install fzf
/usr/local/linuxbrew/Cellar/fzf/0.9.11/install
z dotfiles
cd .dotfiles/
gs
git co home/.fzf.bash
..
cd .dotfiles
gs
gdi
ll
/usr/local/linuxbrew/Cellar/fzf/0.9.11/install
cd .dotfiles
gs
cd home
gdi .fzf.bash
vi .fzf.bash
gdi
gs
gdi
..
..
cat examples.desktop
ll
rm examples.desktop
ll
ii vnc5server
vncserver
vi .vnc/xstartup
vncserver -kill :1
su
su
sudo touch /etc/init.d/vncserver
gs
sudo vi /etc/init.d/vncserver
kill -9 21536
emacs
sudo vi /etc/init.d/vncserver
sudo chmod +x /etc/init.d/vncserver
sudo vi /etc/init.d/vncserver
service vncserver start
service vncserver stop
service vncserver start
service vncserver stop
service vncserver stop
service vncserver stop
mkdir -p /etc/vncserver
sudo mkdir -p /etc/vncserver
cd /etc/vncserver/
touch vncservers.conf
sudo touch vncservers.conf
sudo vi vncservers.conf
service vncserver start
update-rc.d vncserver defaults 99
sudo update-rc.d vncserver defaults 99
sudo reboot now
cat .bash_profile
cd .dotfiles/
gdi
gs
vim home/.bash_profile
cat home/.bash_profile
..
ll
z dotfiles
gs
gdi
gs
git co home/.bash/aliases/emacs.aliases.bash
. home/.bash/aliases/emacs.aliases.bash
exit
vim .dotfiles/home/.bash/aliases/emacs.aliases.bash
gs
z dotfiles
vim --version
gs
cd home
vim home/.fzf.bash
vim .vimrc
vim .vimrc
vim .vimrc
vim .vimrc
vim .vimrc
gdi
gs
..
..
cd .ssh/config
cd .ssh
cat config
ll
cp ../.dotfiles/home/.ssh/config config.bak
rm config
cp config.bak config
github-keygen
ll
ll
cd .backup
..
rm -rf .backup
ll
rm known_hosts_github
rm config
github-keygen --?
github-keygen
github-keygen
..
github-keygen
github-keygen catesandrew
cd .ssh
cp config.bak config
ll
github-keygen catesandrew
ll
rm id_catesandrew@github*
ll
rm known_hosts_github
ll
rm config
dfm install
ll
rm -rf .backup
github-keygen catesandrew
ll
z dotfiles
gs
gdi home/.ssh/config
vim home/.ssh/config
git co home/.ssh/config
gs
cd ../.ssh/
ll
rm config.bak
cat id_catesandrew@github.pub
z dotfiles
vim .git/config
pullff
gs
git add home/.vimrc
gc -m 'Check for lua and neocomplete'
..
cp .dotfiles/home/.gitconfig-local.template .gitconfig-local
cp .dotfiles/home/.gitconfig-private.template .gitconfig-private
vim .gitconfig-local
vim .gitconfig-private
z dotfiles
gc -m 'Check for lua and neocomplete'
gp
gs
gs
git add home/.bash/aliases/emacs.aliases.bash
gc -m 'Remove sudo editor emacs on linux'
gp
gs
cat home/.emacs.d/.gitignore
cd home/.emacs.d/
cp custom.el custom.bak
git rm --cached custom.el
g
gs
gs
cat .gitignore
gs
gc -m 'Remove custom.el from git index'
gs
cat custom.el
gs
gp
gs
cp custom.bak custom.el
gs
rm custom.bak
gs
gdi
brew --prefix fzf
gs
..
gs
brew ls --versions
vim .fzf.bash
. .fzf.bash
which -s brew
which -a brew
man which
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .bash_profile
vim .bashrc
vim .bashrc.load
vim .fzf.bash
vim .bashrc.load
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
. .fzf.bash
vim .fzf.bash
brew --prefix fzf
cat .fzf.bash
. .fzf.bash
vim .fzf.bash
gs
git add .fzf.bash
gs
gc -m 'Add guard around fzf'
gs
gp
gs
gs
gdi
exit
which fasd
which brew
brew ls --version fasd
vim .fzf.bash
which fasd
which brew
rm /usr/local/bin/brew
gdi
z dotfiles
gs
gdi
cd .dotfiles
gdi
gs
cd home
vim .bashrc.load
exit
cd .dotfiles/home
vim .bashrc.load
gdi
vim .bashrc.load
exit
gs
z dotfiles
gs
gs
gdi
gs
git add home/.bashrc.load
gc -m 'Add guards around linuxbrew'
gs
gp
cat .git/config
ll
gs
gdi
cd home
vim .bashrc.load
cp .bashrc.load test
vim test
rm test
vim .bashrc.load
. .bashrc.load
vim .bashrc.load
. .bashrc.load
vim .bashrc.load
. .bashrc.load
brew --prefix cask
ln -s /usr/local/linuxbrew/opt/cask ~/.cask
. .bashrc.load
. .bashrc.load
vim .bashrc.load
. .bashrc.load
vim .bashrc.load
vim .bashrc.load
. .bashrc.load
rm ~/.cask
. .bashrc.load
vim .bashrc.load
cat .fzf.bash
vim .bashrc.load
. .bashrc.load
vim .bashrc.load
. .bashrc.load
gs
gdi
vim .emacs.d/init.el
gs
gs
gdi
ga .
emacs
emacs
gs
gp
cat ~/.ssh/known_hosts_github
gs
gc -m 'Init emacs cask'
gs
gp
gs
em
em --debug-init
cd .emacs.d/
ll
cd .cask
rm -rf *
ll
..
cask install
sudo apt-get install build-essential
gs
cd .cask
rm -rf 24.3.1/
..
cd ~
sudo apt-get build-dep emacs24
mkdir emacs-src
cd emacs-src/
wget http://mirror.team-cymru.org/gnu/emacs/emacs-24.4.tar.gz
wget http://mirror.team-cymru.org/gnu/emacs/emacs-24.5.tar.gz
tar xvf emacs-24.4.tar.gz
tar xvf emacs-24.5.tar.gz
rm *.gz
cd emacs-24.5
./configure
make
sudo make install
..
..
which emacs
emacs
cd .emacs.d/
gs
cd .cask/
rm -rf 24.3.1/
..
gs
cask install
sudo apt-get install editorconfig
cask install
gs
..
pwd
emacs
emacs --version
which emacs
cd .bash/aliases/
cat maven.aliases.bash
cat emacs.aliases.bash
exit
exit
which emacs
emacs --version
emacs
rm -rf emacs-src/
ll
cd .dotfiles/
ll
gs
..
emacs .
gs
ll
cd org/
..
ll
vim
ll
pwd
rm .user.vim
vim .
cat /usr/share/applications/emacs24.desktop
emacs --version
cp /usr/share/applications/emacs24.desktop /usr/share/applications/emacs24.5.desktop
sudo cp /usr/share/applications/emacs24.desktop /usr/share/applications/emacs24.5.desktop
sudo apt-get remove libpam-smbpass
sudo apt-get install libpam-smbpass
pam-auth-update
sudo pam-auth-update
sudo vim /usr/share/applications/emacs24.5.desktop
exit
sudo vim /usr/bin/vncserver
cat .Xauthority
cat .vnc/xstartup
mv .vnc/xstartup .vnc/xstartup2
rm .Xauthority
vncserver
sudo service vncserver restart
ps -A | grep vnc
sudo service vncserver stop
ps -A | grep vnc
kill -9 31140
ps -A | grep vnc
sudo service vncserver start
sudo service vncserver stop
cat .Xauthority
cd .vnc
ll
cat xstartup
cat xstartup2
vim xstartup
sudo service vncserver start
sudo service vncserver stop
vim xstartup
sudo service vncserver start
sudo service vncserver stop
vim xstartup
sudo service vncserver start
rm xstartup2
sudo vim /etc/init.d/vncserver
cat /etc/vncserver/vncservers.conf
vim xstartup
sudo vim /etc/vncserver/vncservers.conf
sudo service vncserver start
sudo service vncserver stop
sudo service vncserver start
sudo service vncserver start
sudo vim /etc/vncserver/vncservers.conf
sudo service vncserver start
sudo service vncserver start
sudo service vncserver stop
sudo service vncserver start
xrandr
sudo service vncserver stop
sudo service vncserver stop
sudo vim /etc/init.d/vncserver
sudo apt-get install gnome-panel gnome-settings-daemon metacity nautilus gnome-terminal
sudo service vncserver start
sudo service vncserver stop
sudo dpkg-reconfigure xserver-xorg
sudo vim /etc/X11/
sudo touch /etc/X11/xorg.conf
sudo vim /etc/X11/xorg.conf
sudo reboot -now
sudo reboot now
sudo vim /etc/X11/xorg.conf
exit
exit
sudo rm /etc/X11/xorg.conf
sudo reboot now
sudo service vncserver stop
sudo update-rc.d vncserver disable
cat /etc/ssh/sshd_config
cat .ssh/config | grep -i x11
cat .ssh/config
echo $DISPLAY
ll
which xauth
exit
exit
exit
exit
pwd
exit
gs
echo $DISPLAY
mv .Xauthority .backup/
cd .vnc
mkdir .backup
ll
cat xstartup
ll
mv passwd .backup/
mv xstartup .backup/
..
sudo reboot now
pkill vino
echo $DISPLAY
export DISPLAY=:0.0
/usr/lib/vino/vino-server
/usr/lib/vino/vino-server
vino-preferences
cat .Xauthority
cat .backup/.Xauthority
cd .vnc/
ll
mv .backup/* .
ll
rm .backup/
..
rm -rf .vnc/.backup/
rm .Xauthority
mv .backup/.Xauthority .
sudo reboot now
sudo dpkg-reconfigure xserver-xorg
sudo reboot now
mvim .bash_history
sudo apt-get purge vnc4server
sudo apt-get update
sudo apt-get purge gnome-panel gnome-settings-daemon meatacity nautilus gnome-terminal
sudo apt-get purge gnome-panel gnome-settings-daemon metacity nautilus gnome-terminal
vim .bash_history
sudo reboot now
sudo dpkg-reconfigure xserver-xorg
sudo reboot now
sudo reboot now
sudo reboot now
gsettings set org.gnome.settings-daemon.plugins.background active true
sudo apt-get install unity-desktop
sudo apt-get install ubuntu-desktop
sudo reboot now
sudo fdisk -l /dev/sdc
sudo umount /dev/sdc
sudo umount /dev/sdc
umount /dev/sdb1
sudo fdisk -l /dev/sdc
df /dev/sdc
sudo df /dev/sdc
ll
ll /dev/sdc1
ll /dev/sdc2
cp .bash_history /dev/sdc1
cp .bash_history /dev/sdcr2
cp .bash_history /dev/sdc2
cp .bash_history bash_history
rm bash_history
