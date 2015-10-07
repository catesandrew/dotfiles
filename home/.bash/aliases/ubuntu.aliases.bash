if [ $(uname) = "Linux" ]; then
    # https://mkaz.com/2013/01/13/ubuntu-guide-for-mac-converts/
    alias pbcopy=&#039;xclip -selection clipboard&#039;
    alias pbpaste=&#039;xclip -selection clipboard -o&#039;
    alias open=xdg-open

    # Alias for getting OpenPGP keys for Launchpad PPAs on Ubuntu
    # http://www.commandlinefu.com/commands/view/2437/alias-for-getting-openpgp-keys-for-launchpad-ppas-on-ubuntu
    alias launchpadkey="sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys"

    # (Debian/Ubuntu) Discover what package a file belongs to
    # http://www.commandlinefu.com/commands/view/1947/debianubuntu-discover-what-package-a-file-belongs-to
    # alias dls="dpkg -S /usr/bin/ls"

    # Find distro name and/or version/release
    # http://www.commandlinefu.com/commands/view/1228/find-distro-name-andor-versionrelease
    # cat /etc/*-release
fi
