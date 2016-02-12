# load packer, if you are using it

if cask_contains_element "packer" || \
    hash packer 2>/dev/null; then
  export PACKER_CACHE_DIR="${HOME}/.packer_cache"
fi
