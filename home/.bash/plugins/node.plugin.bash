cite about-plugin
about-plugin 'Node.js helper functions'

# Set Node environment variables
export NODE_REPL_HISTORY_FILE="$HOME/.node_history"

# Strip other version from PATH
PATH=$(strip_path "$PATH" "\./node_modules/\.bin")

# Prepend `./node_modules/.bin`
path_prepend "./node_modules/.bin"

# tells npm to compile and install all your native addons in parallel and not
# sequentially. This greatly increases installation times.
if hash npm 2>/dev/null; then
    export JOBS=max
fi
