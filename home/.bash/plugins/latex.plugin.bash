# use mactex

# add mactex to the path if its present
MACTEX_PATH=/usr/local/texlive/2015/bin/x86_64-darwin
if [[ -d  $MACTEX_PATH ]]; then
    path_munge "${MACTEX_PATH}" "after"
fi
unset MACTEX_PATH
