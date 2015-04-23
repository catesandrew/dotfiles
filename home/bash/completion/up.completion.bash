#!/usr/bin/env bash

# https://gist.github.com/ttscoff/893afa7acdcd6c696dc7
# Bash completion for `up` <http://brettterpstra.com/2014/05/14/up-fuzzy-navigation-up-a-directory-tree/>
_up_complete()
{
	local rx
	local token=${COMP_WORDS[$COMP_CWORD]}
	local IFS=$'\t'
	local words=$(dirname `pwd` | tr / "	")

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
				matches+=("${w// /\ }")
			fi
		done
	fi

	(( nocasematchWasOff )) && shopt -u nocasematch

	COMPREPLY=("${matches[@]}")
}

complete -F _up_complete up
