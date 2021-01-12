#!/usr/bin/env bash

# This shell script checks my emails
# It first checks that I am connected to the internet by attempting to ping cloudflaire's dns server
# Then checks that davmail is running
# Then syncs emails
# Then updates notmuch

die(){
	echo "$1"
	exit 1
}

export NOTMUCH_CONFIG="$HOME/.notmuch-config"

folder="${1:-all}"

# Ping 1.1.1.1 to confirm that we are on the internet
ping -c 1 "1.1.1.1" > /dev/null 2> /dev/null || die "Need to be connected to the internet"

# Check that davmail is running
ps -aux | grep -v grep | grep -q davmail || die "Need to start davmail"

# Sync maildir with exchange
mbsync -c "$HOME/.config/isync/mbsyncrc.secret" "work-$folder"

# Update notmuch database
notmuch new

new="$(notmuch count "tag:unread")"

[ "$new" -gt "0" ] && notify-send "$new new messages"
