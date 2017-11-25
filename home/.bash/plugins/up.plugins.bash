# up: Quickly cd up a directory tree

# https://gist.github.com/ttscoff/2251ec8d7f477f7d3fd0
# http://brettterpstra.com/2014/05/14/up-fuzzy-navigation-up-a-directory-tree/
# inspired by `bd`: https://github.com/vigneshwaranr/bd

function _up() {
  local rx updir
  rx=$(ruby -e "print '$1'.gsub(/\s+/,'').split('').join('.*?')")
  updir=`echo $PWD | ruby -e "print STDIN.read.sub(/(.*\/${rx}[^\/]*\/).*/i,'\1')"`
  echo -n "$updir"
}

function up() {
  if [ $# -eq 0 ]; then
    echo "up: traverses up the current working directory to first match and cds to it"
    echo "You need an argument"
  else
    cd $(_up "$@")
  fi
}
