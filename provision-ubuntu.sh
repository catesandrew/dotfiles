#!/bin/bash

LIB="${HOME}/.bash/lib"
source "${LIB}/utils.sh"
source "${LIB}/brew.sh"
source "${LIB}/npm.sh"

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
  echo -e "${2}${1}\033[m"
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
      ;;
  *) ;;
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
echo "Would you like to set your computer name? (y/n)"
read -r response
case $response in
  [yY])
      echo "What would you like it to be?"
      read COMPUTER_NAME
      sudo hostname $COMPUTER_NAME
      sudo hostnamectl set-hostname $COMPUTER_NAME
      ;;
  *) ;;
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
echo "Would you like to install git? (y/n)"
read -r response
case $response in
  [yY])

    exec_sudo_cmd 'add-apt-repository -y ppa:git-core/ppa'
    exec_sudo_cmd 'apt-get update'
    exec_sudo_cmd 'apt-get -y install git'
    ;;
  *)
    ;;
esac

# User should belong to adm group
# sudo usermod -aG adm andrew

PREFIX=/usr/local
echo ""
echo "Would you like to setup /usr/local? (y/n)"
read -r response
case $response in
  [yY])
    print_status "Preparing the /usr/local directory"
    exec_sudo_cmd "chgrp adm ${PREFIX}"
    exec_sudo_cmd "chmod g+rwx ${PREFIX}"

    exec_sudo_cmd "chgrp adm ${PREFIX}/*"
    exec_sudo_cmd "chmod g+rwx ${PREFIX}/*"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install the basics? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get -y install curl wget unzip git ack-grep htop vim tmux software-properties-common"
    ;;
  *)
    ;;
esac

###############################################################################
# Linuxbrew
###############################################################################

HOMEBREW_PREFIX="${PREFIX}/linuxbrew"
HOMEBREW_CACHE="${HOMEBREW_PREFIX}/.cache"

echo ""
echo "Install linuxbrew? (y/n)"
read -r response
case $response in
  [yY])
    # dependencies
    exec_sudo_cmd 'apt-get -y install build-essential curl git m4 ruby texinfo libbz2-dev libcurl4-openssl-dev libexpat-dev libncurses-dev zlib1g-dev'

    exec_cmd "git clone https://github.com/Homebrew/linuxbrew.git ${HOMEBREW_PREFIX}"
    exec_sudo_cmd "chmod g+rwx ${HOMEBREW_PREFIX}"
    # the group is set to wheel by default for some reason
    exec_sudo_cmd "chgrp adm ${HOMEBREW_PREFIX}"

    exec_cmd "mkdir -p ${HOMEBREW_CACHE}"
    exec_sudo_cmd "chmod g+rwx ${HOMEBREW_CACHE}"

    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install your dotfiles? (y/n)"
read -r response
case $response in
  [yY])
    DFM_REPO="${HOME}/.dotfiles"
    exec_cmd 'git clone --recursive https://github.com/catesandrew/dotfiles.git ~/.dotfiles'
    exec_cmd 'DFM_REPO=~/.dotfiles ~/.dotfiles/home/.bin/dfm install'
    exec_cmd "cd ${HOME}/.vim/bundle/vimproc"
    exec_cmd 'make'
    exec_cmd 'cd -'
    exec_cmd 'cp "${DFM_REPO}/home/.gitconfig-local.template ${HOME}/.gitconfig-local"'
    exec_cmd 'cp "${DFM_REPO}/home/.gitconfig-private.template ${HOME}/.gitconfig-private"'

    print_status 'Edit the .gitconfig-local and .gitconfig-private files'
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install powerline fonts? (y/n)"
read -r response
case $response in
  [yY])
    TMP_DIR=`mktemp -d`
    exec_cmd "git clone https://github.com/powerline/fonts.git ${TMP_DIR}"
    exec_cmd "${TMP_DIR}/install.sh"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install node via ppa? (y/n)"
read -r response
case $response in
  [yY])
    # curl -sL https://deb.nodesource.com/setup_0.12 | sudo bash -

    print_status "Updating NodeJS PPA..."

    ## NodeSource's Node.js PPA
    exec_sudo_cmd "echo 'deb https://deb.nodesource.com/node_0.12 ${DISTRO} main' > /etc/apt/sources.list.d/nodesource.list"
    exec_sudo_cmd "echo 'deb-src https://deb.nodesource.com/node_0.12 ${DISTRO} main' >> /etc/apt/sources.list.d/nodesource.list"
    exec_sudo_cmd "apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 68576280"

    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get -y install nodejs"

    print_status "Installing native build tools..."
    exec_sudo_cmd "apt-get -y install build-essential"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install heroku via ppa? (y/n)"
read -r response
case $response in
  [yY])
    # add heroku repository to apt
    exec_sudo_cmd "echo 'deb http://toolbelt.heroku.com/ubuntu ./' > /etc/apt/sources.list.d/heroku.list"

    # install heroku's release key for package verification
    exec_sudo_cmd "wget -O- https://toolbelt.heroku.com/apt/release.key | apt-key add -"

    # update your sources
    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get -y heroku-toolbelt"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install emacs snapshot? (y/n)"
read -r response
case $response in
  [yY])
    print_status "Installing emacs snapshot"
    exec_sudo_cmd "add-apt-repository -y ppa:ubuntu-elisp"
    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get -y install emacs-snapshot emacs-snapshot-el"

    # sudo update-alternatives --config emacs
    exec_sudo_cmd "update-alternatives --set emacs /usr/bin/emacs-snapshot"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install nvm? (y/n)"
read -r response
case $response in
  [yY])
    print_status "Installing native build tools..."
    # exec_sudo_cmd "apt-get install -y build-essential libssl-dev"
    exec_cmd "brew install nvm"

    exec_sudo_cmd "mkdir -p ${PREFIX}/nvm"
    exec_sudo_cmd "chgrp adm ${PREFIX}/nvm"
    exec_sudo_cmd "chmod g+rwx ${PREFIX}/nvm"

    export NVM_DIR="${PREFIX}/nvm"

    # Load NVM into a shell session *as a function*
    exec_cmd "source $(brew --prefix nvm)/nvm.sh"

    exec_cmd "nvm install -s v0.12.4"
    exec_cmd "nvm alias default 0.12.4"
    exec_cmd "nvm install iojs-v2.4.0"
    exec_cmd "nvm use default"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to disable smb password synchronization? (y/n)"
read -r response
case $response in
  [yY])
    print_status "Launching pam-auth-update ..."
    print_status "Please uncheck SMB password synchronization ..."
    exec_sudo_cmd "pam-auth-update"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install editorconfig? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get -y install editorconfig"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install apparix? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get -y install apparix"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install lua? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get -y install lua5.2"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install image libs? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get -y install libtiff5-dev libpng12-dev libjpeg-dev libgif-dev libgnutls-dev libxml2-dev"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install fasd? (y/n)"
read -r response
case $response in
  [yY])
    exec_cmd "brew install fasd"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install golang? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get install -y golang"

    exec_sudo_cmd "mkdir -p ${PREFIX}/go"
    exec_sudo_cmd "chgrp adm ${PREFIX}/go"
    exec_sudo_cmd "chmod g+rwx ${PREFIX}/go"
    export GOPATH="${PREFIX}/go"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install ruby2.2? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get install -y software-properties-common"
    exec_sudo_cmd "apt-add-repository -y ppa:brightbox/ruby-ng"
    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get install -y ruby2.2 ruby2.2-dev"
    exec_sudo_cmd "sudo gem2.2 install bundler"
    exec_sudo_cmd "apt-get install -y ruby-switch"
    # exec_sudo_cmd "apt-get purge -y ruby1.9"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to update gem? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "gem update system"
    exec_sudo_cmd "gem install nokogiri"
    # exec_sudo_cmd "gem install chef"
    # nokogiri requirements
    # mkmf is part of the ruby-dev package
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install rbenv? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get install -y rbenv"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install fzf? (y/n)"
read -r response
case $response in
  [yY])
    exec_cmd "git clone https://github.com/junegunn/fzf.git ${PREFIX}/fzf"
    exec_sudo_cmd "chgrp adm ${PREFIX}/fzf"
    exec_sudo_cmd "chmod g+rwx ${PREFIX}/fzf"
    exec_cmd "${PREFIX}/fzf/install"

    # exec_sudo_cmd "curl -sL https://raw.githubusercontent.com/junegunn/fzf/master/install | sudo bash -"
    # exec_cmd "brew install fzf"
    # exec_cmd "$(brew --prefix fzf)/install"
    # exec_cmd "git co $HOME/.fzf.bash"
    # exec_cmd "source $(brew --prefix nvm)/nvm.sh"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install automatic security updates? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get install -y unattended-upgrades"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install ack? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get install -y ack-grep"
    # Renaming ack-grep on Debian-derived distros. On Debian-derived distros, ack is
    # packaged as "ack-grep" because "ack" already existed. Your ack will be called
    # "ack-grep", which is 167% more characters to type per invocation. This is
    # tragic for your poor fingers.

    # To create a local diversion, renaming ack-grep to ack, first install the
    # ack-grep package as shown above. Then, run:
    exec_sudo_cmd "dpkg-divert --local --divert /usr/bin/ack --rename --add /usr/bin/ack-grep"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install ag? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get install -y silversearcher-ag"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install sslmate? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "wget -P /etc/apt/sources.list.d https://sslmate.com/apt/ubuntu1404/sslmate.list"
    exec_sudo_cmd "wget -P /etc/apt/trusted.gpg.d https://sslmate.com/apt/ubuntu1404/sslmate.gpg"
    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get -y install sslmate"

    print_status "Be sure to turn 'sslmate link' to link your account."
    print_status "Download your certs with 'sss download *.domain.com'"
    print_status "Copy your private key 'sudo mv private-pem.key /etc/sslmate/*.domain.com.key'"
    print_status "Then 'sudo chmod 644 /etc/sslmate/*.domain.com.key'"
    print_status "'sudo chown root /etc/sslmate/*.domain.com.key'"
    print_status "'sudo chgrp root /etc/sslmate/*.domain.com.key'"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install Ranger file manager? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get -yqq update"
    exec_sudo_cmd "apt-get install -y ranger caca-utils highlight atool w3m poppler-utils mediainfo"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to uninstall unity desktop? (y/n)"
read -r response
case $response in
  [yY])
    # To remove unity in 14.04
    # http://askubuntu.com/questions/6302/how-can-you-remove-unity
    exec_sudo_cmd "apt-get remove --purge unity"
    exec_sudo_cmd "apt-get remove --purge gnome-shell"
    exec_sudo_cmd "apt-get remove --purge lightdm"
    exec_sudo_cmd "apt-get autoremove"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install vagrant? (y/n)"
read -r response
case $response in
  [yY])
    # If you are running Vagrant on Ubuntu (14.04) and installed through apt-get,
    # the latest version of Vagrant in the Ubuntu repository is 1.4. Downloading the
    # latest (1.7.2 at this time) from the vagrant downloads page resolves the
    # issue. As discussed
    # [here](https://github.com/Varying-Vagrant-Vagrants/VVV/issues/354)

    exec_sudo_cmd "echo deb http://vagrant-deb.linestarve.com/ any main > /etc/apt/sources.list.d/wolfgang42-vagrant.list"
    exec_sudo_cmd "apt-key adv --keyserver pgp.mit.edu --recv-key 2099F7A4"
    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get -y install vagrant"

    print_status "List of vagrant plugins to consider installing"
    print_status "- vagrant-aws"
    print_status "- vagrant-hostmanager"
    print_status "- vagrant-librarian-chef"
    print_status "- vagrant-list"
    print_status "- vagrant-share"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install latest virtualbox via ppa? (y/n)"
read -r response
case $response in
  [yY])
    print_status "Insalling VirtualBox via PPA"

    VBOX_LATEST_VERSION=$(curl http://download.virtualbox.org/virtualbox/LATEST.TXT)
    VBOX_MAJOR_MINOR=$(echo ${VBOX_LATEST_VERSION%.*})
    print_status "Found version ${VBOX_MAJOR_MINOR}"
    exec_sudo_cmd "echo 'deb http://download.virtualbox.org/virtualbox/debian ${DISTRO} contrib' > /etc/apt/sources.list.d/virtualbox.list"
    wget -q http://download.virtualbox.org/virtualbox/debian/oracle_vbox.asc -O- | sudo apt-key add -
    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get -y install dkms virtualbox-${VBOX_MAJOR_MINOR}"

    wget -c "http://download.virtualbox.org/virtualbox/${VBOX_LATEST_VERSION}/Oracle_VM_VirtualBox_Extension_Pack-${VBOX_LATEST_VERSION}.vbox-extpack" -O "/tmp/Oracle_VM_VirtualBox_Extension_Pack-${VBOX_LATEST_VERSION}.vbox-extpack"
    exec_sudo_cmd "VBoxManage extpack uninstall 'Oracle VM VirtualBox Extension Pack'"
    exec_sudo_cmd "VBoxManage extpack cleanup"
    exec_sudo_cmd "VBoxManage extpack install /tmp/Oracle_VM_VirtualBox_Extension_Pack-${VBOX_LATEST_VERSION}.vbox-extpack"
    exec_sudo_cmd "usermod -aG vboxusers $USER"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install latest virtualbox guest additions? (y/n)"
read -r response
case $response in
  [yY])
    print_status "Updating vbox guest additions"

    VBOX_LATEST_VERSION=$(curl http://download.virtualbox.org/virtualbox/LATEST.TXT)
    wget -c "http://download.virtualbox.org/virtualbox/${VBOX_LATEST_VERSION}/VBoxGuestAdditions_${VBOX_LATEST_VERSION}.iso" -O "/tmp/VBoxGuestAdditions_${VBOX_LATEST_VERSION}.iso"

    exec_sudo_cmd "mkdir -p /media/guestadditions; sudo mount -o loop /tmp/VBoxGuestAdditions_${VBOX_LATEST_VERSION}.iso /media/guestadditions"
    exec_sudo_cmd "/media/guestadditions/VBoxLinuxAdditions.run"
    exec_sudo_cmd "umount -l /media/guestadditions"
    exec_sudo_cmd "rm -rf /tmp/VBoxGuestAdditions_${VBOX_LATEST_VERSION}.iso /media/guestadditions"

    print_status "You may safely ignore the message that reads: 'Could not find the X.Org or XFree86 Window System.'"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install firewall rules? (y/n)"
read -r response
case $response in
  [yY])
    print_status "Here are your current list of rules"
    exec_sudo_cmd "iptables -L -v"

    # First, we’ll add the rule to allow all loopback traffic:
    exec_sudo_cmd "iptables -A INPUT -i lo -j ACCEPT"

    # Now let’s add the rule to accept current/established connections:
    exec_sudo_cmd "iptables -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT"

    # Open up our SSH/HTTP/HTTPS ports for remote access:
    exec_sudo_cmd "iptables -A INPUT -p tcp --dport 22 -j ACCEPT"
    exec_sudo_cmd "iptables -A INPUT -p tcp --dport 80 -j ACCEPT"
    exec_sudo_cmd "iptables -A INPUT -p tcp --dport 443 -j ACCEPT"

    # Create new chain
    exec_sudo_cmd "iptables -N LOGGING"
    # Add the “catch all” to LOG any packets which made it this far down the rule chain:
    exec_sudo_cmd "iptables -A INPUT -j LOGGING"

    # Log the packets with a prefix
    exec_sudo_cmd "iptables -A LOGGING -m limit --limit 2/min -j LOG --log-prefix 'IPTables Packet Dropped: ' --log-level 7"
    # Drop those packets
    # Note this is added to the LOGGING chain
    exec_sudo_cmd "iptables -A LOGGING -j DROP"

    # Let’s change the INPUT chain to default to DROP:
    exec_sudo_cmd "iptables -P INPUT DROP"

    print_status "Now here are your current list of rules"
    exec_sudo_cmd "iptables -L -v"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to save your firewall rules after reboot? (y/n)"
read -r response
case $response in
  [yY])
    # On Ubuntu, we can use the iptables-persistent package to do this:
    exec_sudo_cmd "apt-get install -y iptables-persistent"

    # Start the service
    exec_sudo_cmd "service iptables-persistent start"

    # save the rules
    exec_sudo_cmd "iptables-save > /etc/iptables/rules.v4"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install Fail2Ban? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-get install -y fail2ban"
    exec_sudo_cmd "cp /etc/fail2ban/jail.conf /etc/fail2ban/jail.local"
    exec_sudo_cmd "service fail2ban reload"
    exec_sudo_cmd "iptables-save > /etc/iptables/rules.v4"

    # When that’s done, restart iptables-persistent:
    exec_sudo_cmd "service iptables-persistent restart"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install ansible? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "apt-add-repository -y ppa:ansible/ansible"
    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get install -y ansible"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install Docker (y/n)"
read -r response
case $response in
  [yY])
    # Enable memory cgroup and swap accounting for Docker.
    # http://docs.docker.io/en/latest/installation/kernel/#memory-and-swap-accounting-on-debian-ubuntu
    exec_sudo_cmd "sed -i 's/^GRUB_CMDLINE_LINUX=\"\"$/GRUB_CMDLINE_LINUX=\"cgroup_enable=memory swapaccount=1\"/' /etc/default/grub"
    exec_sudo_cmd "update-grub"

    # add docker group
    exec_sudo_cmd "groupadd docker"
    exec_sudo_cmd "usermod -aG docker $USER"
    # Add vagrant to docker group
    # exec_sudo_cmd "usermod -aG docker vagrant"

    # official Ubuntu AMIs create a default user with the username ubuntu
    # sudo usermod -aG docker ubuntu

    # add the docker gpg key
    exec_sudo_cmd "curl https://get.docker.io/gpg | apt-key add -"

    # Add the Docker repository to your apt sources list.
    exec_sudo_cmd "echo deb https://get.docker.io/ubuntu docker main > /etc/apt/sources.list.d/docker.list"
    exec_sudo_cmd "apt-get update"
    exec_sudo_cmd "apt-get install -y lxc-docker"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install GitLab EE? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "curl https://packages.gitlab.com/install/repositories/gitlab/gitlab-ee/script.deb.sh | bash"
    exec_sudo_cmd "apt-get install -y gitlab-ee"

    print_status "Edit the /etc/gitlab/gitlab.rb file to make changes to gitlab"
    print_status "Afterwards run 'sudo gitlab-ctl reconfigure'"
    print_status "Then run 'sudo gitlab-ctl restart'"
    ;;
  *)
    ;;
esac

echo ""
echo "Would you like to install GitLab CI runner? (y/n)"
read -r response
case $response in
  [yY])
    exec_sudo_cmd "curl -L https://packages.gitlab.com/install/repositories/runner/gitlab-ci-multi-runner/script.deb.sh | bash"
    exec_sudo_cmd "apt-get -yqq update"
    exec_sudo_cmd "apt-get install -y gitlab-ci-multi-runner"

    print_status "Register the runner with 'sudo gitlab-ci-multi-runner register'"
    ;;
  *)
    ;;
esac

# disallow remote log in directly as root user with ssh
echo ""
cecho "###############################################" $red
cecho "# Disable remote root login with ssh          #" $red
cecho "###############################################" $red
print_status "Consider disallowing remote login as root with ssh"
print_status "Edit '/etc/ssh/sshd_config' and change the following property to no"
print_status "PermintRootLogin no"
print_status "And restart ssh 'sudo service ssh restart'"

echo ""
cecho "###############################################" $green
cecho "# Cleanup                                     #" $green
cecho "###############################################" $green
exec_sudo_cmd "apt-get -y autoremove"
exec_sudo_cmd "apt-get clean"
exec_sudo_cmd "rm -rf /tmp/*"

# sudo vim /etc/apt/apt.conf.d/10periodic

# /usr/bin/setxkbmap -option "ctrl:swapcaps"
