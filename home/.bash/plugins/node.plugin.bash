cite about-plugin
about-plugin 'Node.js helper functions'

node_strip_path() {
  local NM_DIR="./node_modules"
  echo "$1" | command sed \
    -e "s#$NM_DIR[^/]*$2[^:]*:##g"
}

node_prepend_path() {
  if [ -z "$1" ]; then
    echo "$2"
  else
    echo "$2:$1"
  fi
}

# Make sure the global npm prefix is on the path
[[ `which npm` ]] && pathmunge $(npm config get prefix)/bin

# Strip other version from PATH
PATH="$(node_strip_path "$PATH" "/.bin")"

# Prepend `./node_modules/.bin`
PATH="$(node_prepend_path "$PATH" "./node_modules/.bin")"

# pathmunge ./node_modules/.bin

# tells npm to compile and install all your native addons in parallel and not
# sequentially. This greatly increases installation times.
[[ `which npm` ]] && export JOBS=max
