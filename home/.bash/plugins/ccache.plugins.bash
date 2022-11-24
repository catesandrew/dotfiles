if cask_contains_element "ccache" || \
    hash ccache 2>/dev/null; then

  export CCACHE_HOME=$(dirname $(dirname $(which ccache)))/opt/ccache
  # export CCACHE_DIR="$(git rev-parse --show-toplevel)/.ccache"

  # export CC="${CCACHE_HOME}/libexec/clang"
  # export CXX="${CCACHE_HOME}/libexec/clang++"
  # export CMAKE_C_COMPILER_LAUNCHER=$(which ccache)
  # export CMAKE_CXX_COMPILER_LAUNCHER=$(which ccache)

  # zero statistics counters
  # ccache --zero-stats 1> /dev/null

  # show summary of configuration and statistics counters in human-readable
  # format (use -v/--verbose once or twice for more details)
  # ccache --show-stats --verbose
fi
