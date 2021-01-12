#!/bin/sh

# https://git.jonathanh.co.uk/jab2870/Dotfiles/src/branch/master/bin/.bin/emails/send-from-mutt

#EMAIL=$(pass Email/protonmail | grep BridgeUsername | cut -d':' -f2)
#~/.bin/emails/MIMEmbellish | msmtp --user "$EMAIL" "$@"

# Put the message, send to stdin, in a variable
message="$(cat -)"
config="$HOME/.config/msmtp/config.secret"
# Look at the first argument,
# Use it to determine the account to use
# If not set, assume work
# All remaining arguments should be recipient addresses which should be passed to msmtp
case "$(echo "$1" | tr '[A-Z]' '[a-z]')" in
	"work") account="work"; shift ;;
	"gmail") account="gmail"; shift ;;
	*) account="work"; ;;
esac

cleanHeaders(){
	# In the headers, delete any lines starting with markdown
	cat - | sed '0,/^$/{/^markdown/Id;}'
}

echo "$message" | sed '/^$/q' | grep -q -i 'markdown: true' \
	&& echo "$message" | cleanHeaders | convertToHtmlMultipart | msmtp --file="$config" --account="$account" "$@" \
	|| echo "$message" | cleanHeaders | msmtp --file="$config" --account="$account" "$@"

# Update notmuch database
( sleep 2 && notmuch new ) &
