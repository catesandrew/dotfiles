#!/bin/sh

# - Syncs mail for all accounts, or a single account given as an argument.
# - Displays a notification showing the number of new mails.
# - Displays a notification for each new mail with its subject displayed.
# - Runs notmuch to index new mail.
# - This script can be set up as a cron job for automated mail syncing.

# There are many arbitrary and ugly features in this script because it is
# inherently difficult to pass environmental variables to cronjobs and other
# issues. It also should at least be compatible with Linux (and maybe BSD) with
# Xorg and MacOS as well.

# Run only if user logged in (prevent cron errors)
pgrep -u "${USER:=$LOGNAME}" >/dev/null || { echo "$USER not logged in; sync will not run."; exit ;}
# Run only if not already running in other instance
pgrep -x mbsync >/dev/null && { echo "mbsync is already running." ; exit ;}

# First, we have to get the right variables for the mbsync file, the pass
# archive, notmuch and the GPG home.  This is done by searching common profile
# files for variable assignments. This is ugly, but there are few options that
# will work on the maximum number of machines.

# eval "$(grep -h -- \
# 	"^\s*\(export \)\?\(MBSYNCRC\|PASSWORD_STORE_DIR\|NOTMUCH_CONFIG\|GNUPGHOME\)=" \
# 	"$HOME/.profile" "$HOME/.bash_profile" "$HOME/.zprofile" "$HOME/.bashrc" "$HOME/.zshrc" "$HOME/.pam_environment" 2>/dev/null)"

# One alternative to this kind of command would be marking the script for
# /bin/sh -l. That might cause other problems on other particular setups that
# do more complicated things on login, or those people who assign environmental
# variables in shell rc files.

# export GPG_TTY=$TTY

[ -n "$MBSYNCRC" ] && alias mbsync="mbsync -c $MBSYNCRC" || MBSYNCRC="$HOME/.mbsyncrc"

# Settings are different for MacOS (Darwin) systems.
case "$(uname)" in
	Darwin)
		notify() { osascript -e "display notification \"$2 in $1\" with title \"You've got Mail\" subtitle \"Account: $account\"" && sleep 2 ;}
		messageinfo() { osascript -e "display notification with title \"📧 $from\" subtitle \"$subject\"" ;}
		;;
	*)
		displays="$(pgrep -a Xorg | grep -wo "[0-9]*:[0-9]\+")"
	    	notify() { for x in $displays; do
				export DISPLAY=$x
				notify-send --app-name="mutt" "mutt-wizard" "📬 $2 new mail(s) in \`$1\` account."
			done ;}
		messageinfo() { for x in $displays; do
				export DISPLAY=$x
				notify-send --app-name="mutt" "📧$from:" "$subject"
			done ;}
		;;
esac

# Check account for new mail. Notify if there is new content.
syncandnotify() {
    acc="$(echo "$account" | sed "s/.*\///")"
    if [ -z "$opts" ]; then mbsync "$acc"; else mbsync "$opts" "$acc"; fi
    new=$(find "$HOME/.mail/$acc/INBOX/new/" "$HOME/.mail/$acc/Inbox/new/" "$HOME/.mail/$acc/inbox/new/" -type f -newer "$HOME/.mutt/.mailsynclastrun" 2> /dev/null)
    newcount=$(echo "$new" | sed '/^\s*$/d' | wc -l)
    if [ "$newcount" -gt "0" ]; then
	notify "$acc" "$newcount" &
        for file in $new; do
            # Extract subject and sender from mail.
            from=$(awk '/^From: / && ++n ==1,/^\<.*\>:/' "$file" | perl -CS -MEncode -ne 'print decode("MIME-Header", $_)' | awk '{ $1=""; if (NF>=3)$NF=""; print $0 }' | sed 's/^[[:blank:]]*[\"'\''\<]*//;s/[\"'\''\>]*[[:blank:]]*$//')
            subject=$(awk '/^Subject: / && ++n == 1,/^\<.*\>: / && ++i == 2' "$file" | head -n 1 | perl -CS -MEncode -ne 'print decode("MIME-Header", $_)' | sed 's/^Subject: //' | sed 's/^{[[:blank:]]*[\"'\''\<]*//;s/[\"'\''\>]*[[:blank:]]*$//' | tr -d '\n')
	    messageinfo &
        done
    fi
}

# Sync accounts passed as argument or all.
if [ "$#" -eq "0" ]; then
    accounts="$(awk '/^IMAPAccount/ {print $2}' "$MBSYNCRC")"
else
    for arg in "$@"; do
        [ "${arg%${arg#?}}" = '-' ] && opts="${opts:+${opts} }${arg}" && shift 1
    done
    accounts=$*
fi

# Parallelize multiple accounts
for account in $accounts; do
    syncandnotify &
done

wait

# notmuch new 2>/dev/null

#Create a touch file that indicates the time of the last run of mailsync
touch "$HOME/.mutt/.mailsynclastrun"
