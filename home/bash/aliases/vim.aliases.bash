cite 'about-alias'
about-alias 'vim abbreviations'

alias v='mvim --remote-tab'

function gvim() {
    local DIR_NAME=`git rev-parse --show-toplevel`
    echo "Running mvim --servername `basename ${DIR_NAME}`"
    mvim --servername `basename ${DIR_NAME}`
}
