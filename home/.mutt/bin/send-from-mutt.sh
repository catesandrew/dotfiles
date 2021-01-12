#!/bin/sh

# Put the message, send to stdin, in a variable
message="$(cat -)"

# Look at the first argument, use it to determine the account to use, otherwise assume work.
# All remaining arguments should be recipient addresses which should be passed to msmtp.
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
	&& echo "$message" | cleanHeaders | convert-to-html-multipart | msmtp --account="$account" "$@" \
	|| echo "$message" | cleanHeaders | msmtp --account="$account" "$@"

# Update notmuch database
( sleep 2 && notmuch new ) &
