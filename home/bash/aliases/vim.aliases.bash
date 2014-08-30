cite 'about-alias'
about-alias 'vim abbreviations'

alias v='mvim --remote-tab'

function mvim2() {
    DIR_NAME=`git rev-parse --show-toplevel`
    mvim --servername `basename ${DIR_NAME}` .
}
