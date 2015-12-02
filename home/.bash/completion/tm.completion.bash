#!/bin/bash

# Fuzzy Bash completion for tmux session/window using [tm](http://brettterpstra.com/2014/05/11/making-my-tmux-life-easier/)
# https://gist.github.com/ttscoff/03402e2be664bdbd75c4

_tm_complete() {
	local rx
	local token=${COMP_WORDS[$COMP_CWORD]}
	local IFS=$'\t'
	local words
	if [ $COMP_CWORD -eq 2 ]; then
		words=$(tmux list-windows -t ${COMP_WORDS[1]} 2> /dev/null | awk '{print $2}' | tr -d '*-' | tr "\n" "\t")
	elif [ $COMP_CWORD -eq 1 ]; then
		words=$(tmux -q list-sessions 2> /dev/null | cut -f 1 -d ':' | tr "\n" "	")
	fi

	local nocasematchWasOff=0
	shopt nocasematch >/dev/null || nocasematchWasOff=1
	(( nocasematchWasOff )) && shopt -s nocasematch

	local w matches=()

	if [[ $token == "" ]]; then
		matches=($words)
	else
		for w in $words; do
			rx=$(ruby -e "print '$token'.gsub(/\s+/,'').split('').join('.*')")
			if [[ "$w" =~ $rx ]]; then
				matches+=($w)
			fi
		done
	fi

	(( nocasematchWasOff )) && shopt -u nocasematch

	COMPREPLY=("${matches[@]}")
}

complete -F _tm_complete tm
