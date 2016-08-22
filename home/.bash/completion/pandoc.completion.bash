#!/bin/bash

# This script enables bash autocompletion for pandoc.  To enable
# bash completion, add this to your .bashrc:
# eval "$(pandoc --bash-completion)"

_pandoc()
{
    local cur prev opts lastc informats outformats datadir
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # These should be filled in by pandoc:
    opts="-f -r --from --read -t -w --to --write -o --output --data-dir -R --parse-raw -S --smart --old-dashes --base-header-level --indented-code-classes -F --filter --normalize -p --preserve-tabs --tab-stop --track-changes --file-scope --extract-media -s --standalone --template -M --metadata -V --variable -D --print-default-template --print-default-data-file --dpi --no-wrap --wrap --columns --toc --table-of-contents --toc-depth --no-highlight --highlight-style -H --include-in-header -B --include-before-body -A --include-after-body --self-contained --html-q-tags --ascii --reference-links --atx-headers --chapters -N --number-sections --number-offset --no-tex-ligatures --listings -i --incremental --slide-level --section-divs --default-image-extension --email-obfuscation --id-prefix -T --title-prefix -c --css --reference-odt --reference-docx --epub-stylesheet --epub-cover-image --epub-metadata --epub-embed-font --epub-chapter-level --latex-engine --latex-engine-opt --bibliography --csl --citation-abbreviations --natbib --biblatex -m --latexmathml --asciimathml --mathml --mimetex --webtex --jsmath --mathjax --katex --katex-stylesheet --gladtex --trace --dump-args --ignore-args --verbose --bash-completion -v --version -h --help"
    informats="native json markdown markdown_strict markdown_phpextra markdown_github markdown_mmd commonmark rst mediawiki docbook opml org textile html latex haddock twiki docx odt t2t epub"
    outformats="pdf native json docx odt epub epub3 fb2 html html5 icml s5 slidy slideous dzslides revealjs docbook docbook5 opml opendocument latex beamer context texinfo man markdown markdown_strict markdown_phpextra markdown_github markdown_mmd plain rst mediawiki dokuwiki zimwiki textile rtf org asciidoc haddock commonmark tei"
    datadir="/usr/local/homebrew/Cellar/pandoc/1.17.2/share/x86_64-osx-ghc-8.0.1/pandoc-1.17.2"

    case "${prev}" in
         --from|-f|--read|-r)
             COMPREPLY=( $(compgen -W "${informats}" -- ${cur}) )
             return 0
             ;;
         --to|-t|--write|-w|-D|--print-default-template)
             COMPREPLY=( $(compgen -W "${outformats}" -- ${cur}) )
             return 0
             ;;
         --email-obfuscation)
             COMPREPLY=( $(compgen -W "references javascript none" -- ${cur}) )
             return 0
             ;;
         --latex-engine)
             COMPREPLY=( $(compgen -W "pdflatex lualatex xelatex" -- ${cur}) )
             return 0
             ;;
         --print-default-data-file)
             COMPREPLY=( $(compgen -W "reference.odt reference.docx $(find ${datadir} | sed -e 's/.*\/data\///')" -- ${cur}) )
             return 0
             ;;
         --highlight-style)
             COMPREPLY=( $(compgen -W "pygments tango espresso zenburn kate monochrome haddock" -- ${cur}) )
             return 0
             ;;
         *)
             ;;
    esac

    case "${cur}" in
         -*)
             COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
             return 0
             ;;
         *)
             COMPREPLY=( $(compgen -f ${cur}) )
             return 0
             ;;
    esac

}

complete -F _pandoc pandoc

