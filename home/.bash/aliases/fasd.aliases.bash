cite 'about-alias'
about-alias 'fasd abbreviations'

# Fasd can mimic v's behavior by this alias:
alias v='f -t -e vim -b viminfo'


# Jumping around the filesystem

# First, I use a version of the fasd_cd function included with fasd to jump to
# often used directories. I added some command line completion, which seems
# redundant here, or just dumb, but I do sometimes find myself pressing tab out
# of habit and I like for completion to work.

j() {
    local dir="$(fasd -ld "$@")"
    [[ -d "$dir" ]] && pushd "$dir"
}
complete -d j

# The next function uses fasd to sort directories by frecency, fzf to drill
# down, and then pushd to change to the selected directory. It replaces your
# home directory name with ~ just for aesthetics.

jj() {
    local dir
    dir=$(fasd -Rdl |\
        sed "s:$HOME:~:" |\
        fzf --no-sort +m -q "$*" |\
        sed "s:~:$HOME:")\
    && pushd "$dir"
}
complete -d jj

# Finally, two functions substantially taken from the FZF documentation. The
# first changes to a directory chosen using fzf; the difference between this
# and jj above is that jj populates the list with your most often/most recently
# used directories at the bottom, whereas this uses find to list all
# subdirectories of a given (or the current) directory. The second function
# changes to the directory containing a selected file. I don’t use it often,
# but it’s cool when you need it.

jd() {
    local dir
    dir=$(find ${1:-*} -path '*/\.*'\
        -prune -o -type d\
        -print 2> /dev/null | fzf +m)
    [ -d "$dir" ] && pushd "$dir"
}
complete -d jd

jf() {
    local file
    local dir
    file=$(fzf +m -q "$1")\
        && dir=$(dirname "$file")
    [ -d "$dir" ] && pushd "$dir"
}
complete -f jf
