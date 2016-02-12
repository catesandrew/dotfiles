# 'hg helper functions'

# about 'displays dirty status of hg repository'
hg_dirty() {
    hg status --no-color 2> /dev/null \
    | awk '$1 == "?" { print "?" } $1 != "?" { print "!" }' \
    | sort | uniq | head -c1
}

# about 'determine if pwd is an hg repo'
hg_in_repo() {
    [[ `hg branch 2> /dev/null` ]] && echo 'on '
}

# about 'display current hg branch'
hg_branch() {
    hg branch 2> /dev/null
}
