if [ "$__dot_system_type" == "Darwin" ]; then
    alias cwd=__cwd

    function __cwd {
        echo -n "$(pwd)" | pbcopy; echo "Copied '$(pwd)'"
    }

    # Requires growlnotify, which can be found in the Growl DMG under "Extras"
    alias grnot='growlnotify -s -t Terminal -m "Done"'

    # Get rid of those pesky .DS_Store files recursively
    alias dsclean='find . -type f -name .DS_Store -delete'

    # Track who is listening to your iTunes music
    # alias whotunes='lsof -r 2 -n -P -F n -c iTunes -a -i TCP@`hostname`:3689'

    # IP addresses
    alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
    alias localip="ipconfig getifaddr en0"

    # Copy my public key to the pasteboard
    # alias pubkey="more ~/.ssh/id_rsa.pub | pbcopy | printf '=> Public key copied to pasteboard.\n'"

    # Show/hide hidden files (for Mac OS X Mavericks)
    alias showhidden="defaults write com.apple.finder AppleShowAllFiles TRUE"
    alias hidehidden="defaults write com.apple.finder AppleShowAllFiles FALSE"

    # Flush DNS cache
    alias flushdns="dscacheutil -flushcache"

    # Empty the Trash on all mounted volumes and the main HDD
    # Also, clear Apple’s System Logs to improve shell startup speed
    alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

    # Show/hide hidden files in Finder
    alias showdotfiles="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
    alias hidedotfiles="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"

    # Hide/show all desktop icons (useful when presenting)
    alias showdeskicons="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"
    alias hidedeskicons="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"

    alias osxup="sudo softwareupdate -i -a"
    alias fixpref='killall -u $USER cfprefsd'

    # Xcode

    ## Open Xcode workspace from terminal.
    alias xcworkspace='open -a "/Applications/Xcode.app" *.xcworkspace'

    ## Open Xcode project from terminal.
    alias xcproject='open -a "/Applications/Xcode.app" *.xcodeproj'

    export XCODE_10_PATH='/Applications/Xcode10.3.app'
    export XCODE_11_PATH='/Applications/Xcode.app'

    xcselect() {
      sudo xcode-select -s "$1"/Contents/Developer
      ln -s "$1" /Applications/Xcode.app
      echo "Selected Xcode: ${1}"
    }

    alias xcstable="xcselect ${XCODE_11_PATH}"
    alias xcold="xcselect ${XCODE_10_PATH}"
    alias xcprint="xcode-select -p"
fi
