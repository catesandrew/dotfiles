# use mactex

# add mactex to the path if its present
MACTEX_PATH=/usr/local/texlive/2018/bin/x86_64-darwin
if [[ -d  $MACTEX_PATH ]]; then
    path_munge "${MACTEX_PATH}" "after"
fi
unset MACTEX_PATH

# mkdir -p $(kpsewhich --var-value TEXMFHOME)/tex/latex/name
# mkdir -p $(kpsewhich --var-value TEXMFHOME)/makeindex/name
# mkdir -p $(kpsewhich --var-value TEXMFHOME)/doc/latex/name

# the native TeX Live Manager
# tlmgr conf

# Rebuild and manage TeX fmts and Metafont bases, collectively called
# "formats" here. (MetaPost no longer uses the past-equivalent "mems".)
# fmtutil-sys --all

# acrotex If  that folder is not  found, you will need to  create one by
# opening  a terminal  window and  typing  the following.  This creates  a
# `tex/latex` folder  to place the `.sty`  and `.cls` files that  you will
# generate plus an `acrotex` folder for the documentation.
#
# mkdir -p $(kpsewhich --var-value TEXMFHOME)/tex/latex
# mkdir -p $(kpsewhich --var-value TEXMFHOME)/doc/tex/latex/acrotex

# Or  you  can  create  the  folders   in  the  finder,  but  the  above
# is   faster. Once you've   copied   the   `acrotex`   folder   to
# `$(kpsewhich --var-value TEXMFHOME)/tex/latex`,

# cd $(kpsewhich --var-value TEXMFHOME)/tex/latex/acrotex
# latex acrotex.ins

# This should generate all of the appropriate `.sty` and `.cls` files.

# To make the documentation findable by `texdoc`, do the following:

# cp doc/* ~/Library/texmf/doc/tex/latex/acrotex

## LawTex

# cp *.cls bluebook.sty $(kpsewhich --var-value TEXMFHOME)/tex/latex/lawtex
# cp *.ist $(kpsewhich --var-value TEXMFHOME)/makeindex/lawtex
# cp -R *doc* samples $(kpsewhich --var-value TEXMFHOME)/doc/latex/lawtex
#
# Finish off by updating the filename database
#
# mktexlsr
