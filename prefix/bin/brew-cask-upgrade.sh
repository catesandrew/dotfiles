#!/bin/sh

# https://gist.github.com/c00kiemon5ter/3730069b6c920841a3ca#file-brew-cask-upgrade-sh

help=0
latest=0
verbose=0
status=0

usage() {
	cat <<-EOF
	${0##*/} [options]

	  options:
	    -h  show help dialog
	    -l  reinstall packages with version "latest"
	    -v  verbose output
	EOF
	exit "$status"
}

for opt
do case "$opt" in
	'-h') usage ;;
	'-l') latest=1 ;;
	'-v') verbose=1 ;;
	*) status=1 usage ;;
esac
done

set -- $(brew cask list)
sentinel='/'

oldIFS="$IFS"
IFS="$sentinel"
apps="$sentinel$*$sentinel"
IFS="$oldIFS"

for appdir in /usr/local/caskroom/*
do
    echo $appdir
	[ -d "$appdir" ] || continue
    echo $appdir
	app="${appdir##*/}"

	verlocal="$(find "$appdir"/* -type d -print -quit)"
	verlocal="${verlocal##*/}"
	verlatest="$(brew cask info "$app" | awk -v app="$app" '$1 == app":" { print $2; exit }')"

	case "$apps" in
		*"$sentinel$app$sentinel"*)
			if [ "$verbose" -ne 0 ]
			then
				printf -- ':: found app: %s\n' "$app"
				printf -- 'local  version: %s\n' "$verlocal"
				printf -- 'latest version: %s\n' "$verlatest"
			fi
			if [ "$latest" -ne 0 ] && [ "$verlocal" = 'latest' ] || [ "$verlocal" != "$verlatest" ]
			then brew cask install --force "$app" && [ "$verlocal" != "$verlatest" ] && rm -rf "$appdir/$verlocal"
			fi
			;;
		*)
			printf -- 'app not found: %s\n' "$app"
			status=1
			;;
	esac
done

exit "$status"


# https://github.com/caskroom/homebrew-cask/issues/309
function cask_update() {
    rm -rf "$(brew --cache)"
    local caskApps=$(ls $BREW_HOME/caskroom) # Lists the casks in the Caskroom

    for app in ${caskApps}; do # For every app there, do this
        appToCheck=$(brew cask list | grep "${app}") # If the app is not present in `brew cask list`, this variable will be empty

        if [[ -z "${appToCheck}" ]]; then # If the variable is empty, then
            brew cask install --force "${app}" # Force an install of the app
        fi
    done
}

function cask_reinstall() {
    rm -rf "$(brew --cache)"

    for app in $(brew cask list); do
        brew cask install --force "${app}"
    done
}

