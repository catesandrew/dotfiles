# Use xelatex instead of pdflatex
$pdflatex = 'xelatex -interaction=nonstopmode -synctex=1 --shell-escape %O %S';
# examples that were ultimately not used for working with emacs
# $pdflatex = 'xelatex -interaction=nonstopmode -synctex=1 --shell-escape %O %S && (cp "%D" "%R.pdf")';
# $pdflatex = 'xelatex -interaction=nonstopmode -synctex=1 --shell-escape %O %S && (cp "%D" "%R.pdf"); echo Output file copied from "%D" to "%R.pdf" in current directory)';

# $pdflatex = "pdflatex -interaction=nonstopmode -synctex=1 --shell-escape %O %S";

# Always create PDFs
$pdf_mode = 1;

# Use Skim.app to preview generated PDFs
$pdf_previewer = 'open -a Skim.app %S';

# Use continous mode by default
#$preview_continuous_mode = 1;

# output to the directory called build in the current directory
$out_dir = './build';
#$out_dir = "build";

# file extensions to remove when cleaning
$clean_ext = 'acn acr alg aux bbl bcf blg brf fdb_latexmk glg glo gls idx ilg ind ist lof log lot out run.xml toc dvi';

# %B base of filename for current command. E.g., if a postscript file
#    document.ps is being made from the dvi file document.dvi, then the basename
#    is document.
#    "build/arc-08-our-family-wizard-messages"
# %D destination file (e.g., the name of the postscript file when converting
#    a dvi file to postscript).
#    "build/arc-08-our-family-wizard-messages.ind"
# %O options
# %R root filename. This is the base name for the main tex file.
#    "arc-08-our-family-wizard-messages"
# %S source file (e.g., the name of the dvi file when converting a dvi file to ps).
#    "build/arc-08-our-family-wizard-messages.idx"
# %T The name of the primary tex file.
#    "arc-08-our-family-wizard-messages.tex"
# %Y Name of directory for auxiliary output files (see the configuration
#    variable $aux_dir). A directory separation character (’/’) is appended if
#    $aux_dir is non-empty and does not end in a suitable character, with suitable
#    characters being those appropriate to UNIX and MS-Windows, i.e., ’:’, ’/’ and
#    ’\’.
#    "./build/"
# %Z Name of directory for output files (see the configuration variable
#    $out_dir). A directory separation character (’/’) is appended if $out_dir is
#    non-empty and does not end in a suitable character, with suitable characters
#    being those appropriate to UNIX and MS-Windows, i.e., ’:’, ’/’ and ’\’.
#    "./build/"


# http://muug.ca/mirror/ctan/support/latexmk/example_rcfiles/pdflatexmkrc

# Custom dependency and function for nomencl package
add_cus_dep( 'nlo', 'nls', 0, 'makenlo2nls' );
sub makenlo2nls {
  system( "makeindex -s nomencl.ist -o \"$_[0].nls\" \"$_[0].nlo\"" );
}

# Custom dependency for glossary/glossaries package
# if you make custom glossaries you may have to add items to the @cus_dep_list
# and corresponding sub-routines
add_cus_dep( 'glo', 'gls', 0, 'makeglo2gls' );
sub makeglo2gls {
    system( "makeindex -s \"$_[0].ist\" -t \"$_[0].glg\" -o \"$_[0].gls\" \"$_[0].glo\"" );
}

# The glossaries package, with the [acronym] option, produces a .acn file when
# processed with (xe/pdf)latex and then makeindex to process the .acn into .acr
# and finally runs of (xe/pdf)latex to read in the .acr file. Unfortunately the
# glossary package does just the reverse; i.e. (xe/pdf)latex processing
# produces a .acr files and makeindex then is used to convert the .acr file to
# a .acn file which is then ... . This dependency assumes the glossaries
# package.
add_cus_dep( 'acn', 'acr', 0, 'makeacn2acr' );
sub makeacn2acr {
    system( "makeindex -s \"$_[0].ist\" -t \"$_[0].alg\" -o \"$_[0].acr\" \"$_[0].acn\"" );
}

# for acronyms in makeglossaries
# add_cus_dep('acn', 'acr', 0, 'makeglossaries');
# sub acn2acr {
#     system("makeindex $_[0].acn -s $_[0].ist -t $_[0].alg -o $_[0].acr");
# }

# for glossary package (Sigh...) --- they can co-exist!
add_cus_dep( 'acr', 'acn', 0, 'makeacr2acn' );
sub makeacr2acn {
    system( "makeindex -s \"$_[0].ist\" -t \"$_[0].alg\" -o \"$_[0].acn\" \"$_[0].acr\"" );
}

# example of an added custom glossary type that is used in some of the
# glossary/glossaries example files: this is for the new glossary type command
# \newglossary[nlg]{notation}{not}{ntn}{Notation} from the glossaries package
# NOTE: the glossary package uses a very different command: the <in-ext> and
# <out-ext> are reversed in the calling sequence :-(
add_cus_dep( 'ntn', 'not', 0, 'makentn2not' );
sub makentn2not {
    system("makeindex -s \"$_[0].ist\" -t \"$_[0].nlg\" -o \"$_[0].not\" \"$_[0].ntn\"" );
}

# for the	glossary package (Sigh...) --- they can co-exist!
add_cus_dep( 'not', 'ntn', 0, 'makenot2ntn' );
sub makenot2ntn {
    system("makeindex -s \"$_[0].ist\" -t \"$_[0].nlg\" -o \"$_[0].ntn\" \"$_[0].not\"" );
}

# dependencies for custom indexes using the index package
# examples for sample.tex for index package:
add_cus_dep( 'adx', 'and', 0, 'makeadx2and' );
sub makeadx2and {
    system( "makeindex -o \"$_[0].and\" \"$_[0].adx\"" );
}

add_cus_dep( 'ndx', 'nnd', 0, 'makendx2nnd' );
sub makendx2nnd {
    system( "makeindex -o \"$_[0].nnd\" \"$_[0].ndx\"" );
}

add_cus_dep( 'ldx', 'lnd', 0, 'makeldx2lnd' );
sub makeldx2lnd {
    system( "makeindex -o \"$_[0].lnd\" \"$_[0].ldx\"" );
}

# Custom dependency and function for nomencl package
add_cus_dep( 'nlo', 'nls', 0, 'makenlo2nls' );
sub makenlo2nls {
    system( "makeindex -s nomencl.ist -o \"$_[0].nls\" \"$_[0].nlo\"" );
}

# Custom dependency and function(s) for epstopdf package

# FOR USERS OF epstopf v1.4 and before: should also work with v1.5 and later
# note: you may get extras runs if you use the .eps extension in the
# \includgraphics command deletes an outdated pdf-image, and triggers
# a pdflatex-run
# add_cus_dep( 'eps', 'pdf', 0, 'cus_dep_delete_dest' );

# FOR USERS OF epstopdf v1.5 and later only:
# load it as \usepackage[update,prepend]{epstopdf}
# detects an outdated pdf-image, and triggers a pdflatex-run
#add_cus_dep( 'eps', 'pdf', 0, 'cus_dep_require_primary_run' );

# Custom dependecy to convert tif to png
add_cus_dep( 'tif', 'png', 0, 'maketif2png' );
sub maketif2png {
    system( "convert \"$_[0].tif\" \"$_[0].png\"" );
}

# add_cus_dep('asy', 'eps', 0, 'asyhack');
# add_cus_dep('asy', 'pdf', 0, 'asyhack');
# add_cus_dep('asy', 'tex', 0, 'asyhack');
#
# sub asyhack {
#     return system("asy -o asypdf/ '$_'");
# }
