#!/bin/bash
#
# https://gist.github.com/SimonSimCity/13832e2e03597a6be793#file-brew-update-notifier-sh
#
# Notify of Homebrew updates via Notification Center on Mac OS X
#
# Author: Chris Streeter http://www.chrisstreeter.com
# Requires: terminal-notifier. Install with:
#   brew install terminal-notifier
#
# Assuming the script is at `~/.bin/brew-update-notifier.sh`, you can install the
# script to a crontab by running `sudo crontab -e`, then adding the line `0 12
# * * * /Users/<username>/.bin/brew-update-notifier `to the end of the file
# (substituting <username> for your username, or wherever you've put the
# script). I've chosen to run the script every day at noon because my computer
# is likely to be on and connected to a network.
#
# With a launchd-agent it is also run when the Mac is started later.
# Cron-jobs just run if you mac is switched on at that time.
#
# For some help around cronjob vs launchd, I recommend reading this or a similar
# introduction: http://alvinalexander.com/mac-os-x/mac-osx-startup-crontab-launchd-jobs

BREW_EXEC='/usr/local/bin/brew'
TERMINAL_NOTIFIER=`which terminal-notifier`
NOTIF_ARGS="-sender com.apple.Terminal"

$BREW_EXEC update 2>&1 > /dev/null
outdated=`$BREW_EXEC outdated --quiet`
pinned=`$BREW_EXEC list --pinned`

# Remove pinned formulae from the list of outdated formulae
outdated=`comm -1 -3 <(echo "$pinned") <(echo "$outdated")`

if [ -z "$outdated" ] ; then
    if [ -e $TERMINAL_NOTIFIER ]; then
        # No updates available
        $TERMINAL_NOTIFIER $NOTIF_ARGS \
            -title "No Homebrew Updates Available" \
            -message "No updates available yet for any homebrew packages."
    fi
else
    # We've got an outdated formula or two

    # Nofity via Notification Center
    if [ -e $TERMINAL_NOTIFIER ]; then
        lc=$((`echo "$outdated" | wc -l`))
        outdated=`echo "$outdated" | tail -$lc`
        message=`echo "$outdated" | head -5`
        if [ "$outdated" != "$message" ]; then
            message="Some of the outdated formulae are:
$message"
        else
            message="The following formulae are outdated:
$message"
        fi
        # Send to the Nofication Center
        $TERMINAL_NOTIFIER $NOTIF_ARGS \
            -title "Homebrew Update(s) Available" -message "$message"
    fi
fi
