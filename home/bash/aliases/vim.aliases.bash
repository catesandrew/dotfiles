cite 'about-alias'
about-alias 'vim abbreviations'

function gvim() {
    local DIR_NAME=`git rev-parse --show-toplevel`
    echo "Running mvim --servername `basename ${DIR_NAME}`"
    mvim --servername `basename ${DIR_NAME}`
}
