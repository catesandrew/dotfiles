# Select the current directory in launchbar, optionally a file
lb () {
    if [[ $# = 1 ]]; then
        [[ -e "$(pwd)/$1" ]] && open "x-launchbar:select?file=$(pwd)/$1" || open "x-launchbar:select?file=$1"
    else
        open "x-launchbar:select?file=$(pwd)"
    fi
}
