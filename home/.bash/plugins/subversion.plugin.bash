# svn helper functions

# remove ".svn" files from directory
# param '1: directory to search for files'
function svn-rm (){
  if [ -z "$1" ]; then
    reference svn-rm
    return
  fi
  find $1 -name .svn -print0 | xargs -0 rm -rf
}

# add to svn repo
svn-add() {
    svn status | grep '^\?' | sed -e 's/? *//' | sed -e 's/ /\ /g' | xargs svn add
}
