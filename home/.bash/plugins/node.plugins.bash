# Node.js helper functions

# Set Node environment variables
export NODE_REPL_HISTORY_FILE="$HOME/.node_history"
# Allow 32³ entries; the default is 1000.
export NODE_REPL_HISTORY_SIZE='32768'
# Use sloppy mode by default, matching web browsers.
export NODE_REPL_MODE='sloppy'

# Strip other version from PATH
PATH=$(path_strip "$PATH" "\./node_modules/\.bin")

# Prepend `./node_modules/.bin`
path_prepend "./node_modules/.bin"

# tells npm to compile and install all your native addons in parallel and not
# sequentially. This greatly increases installation times.
if hash npm 2>/dev/null; then
  export JOBS=max
fi
