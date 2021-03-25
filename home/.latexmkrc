# Use xelatex instead of pdflatex
$pdflatex = 'xelatex -interaction=nonstopmode -synctex=1 --shell-escape %O %S';
# examples that were ultimately not used for working with emacs
# $pdflatex = 'xelatex -interaction=nonstopmode -synctex=1 --shell-escape %O %S && (cp "%D" "%R.pdf")';
# $pdflatex = 'xelatex -interaction=nonstopmode -synctex=1 --shell-escape %O %S && (cp "%D" "%R.pdf"); echo Output file copied from "%D" to "%R.pdf" in current directory)';

# $pdflatex = "pdflatex -interaction=nonstopmode -synctex=1 --shell-escape %O %S";

# Always create PDFs
$pdf_mode = 1;
# This shows how to use xelatex (http://en.wikipedia.org/wiki/XeTeX)
# with latexmk.  Xelatex uses Unicode and "supporting modern font
# technologies such as OpenType or Apple Advanced Typography.
#
#   WARNING: The method shown here is suitable only for ver. 4.51 and
#            later of latexmk, not for earlier versions.
#
#
# $pdf_mode = 5;
# $postscript_mode = $dvi_mode = 0;


# Use Skim.app to preview generated PDFs
$pdf_previewer = 'open -a Skim.app %S';

# Use continous mode by default
#$preview_continuous_mode = 1;

# output to the directory called build in the current directory
$out_dir = './build';
#$out_dir = "build";

# file extensions to remove when cleaning
$clean_ext = 'acn acr alg aux bbl bcf blg brf fdb_latexmk glg glo gls idx ilg ind ist lof log lot out run.xml toc dvi xdy';

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

# N.B. There is also the OBSOLETE glossary package
# (http://www.ctan.org/pkg/glossary), which has some differences.  See item 2.

# 1. If you use the glossaries or the glossaries-extra package, then you use:

add_cus_dep( 'acn', 'acr', 0, 'makeglossaries' );
add_cus_dep( 'glo', 'gls', 0, 'makeglossaries' );
sub makeglossaries {
   my ($base_name, $path) = fileparse( $_[0] );
   pushd $path;
   my $return = system "makeglossaries", $base_name;
   popd;
   return $return;
}

# 2. If you use the OBSOLETE glossary package, then you can do the following:
#    (Note that the code lines are commented out to avoid trouble when this
#    file is simply copied into a latexmkrc or this file is arranged to be
#    read by latexmk, and one of the modern packages glossaries and
#    glossaries-extra is used.)

## For the main glossary:
#add_cus_dep( 'glo', 'gls', 0, 'makeglo2gls' );
#sub makeglo2gls {
#    system("makeindex -s \"$_[0].ist\" -t \"$_[0].glg\" -o \"$_[0].gls\" \"$_[0].glo\"" );
#}

## For acronyms:
##
## ===> WARNING: The code below is ONLY FOR PACKAGE glossary, NOT FOR
##      glossaries and glossaries-extra. In the current glossaries and
##      glossaries-extra packages the roles of the .acr and .acn files are
##      exchanged compared with the old glossary package.  Hence the the
##      code below will fail with the more modern packages.
#add_cus_dep( 'acr', 'acn', 0, 'makeacr2acn' );
#sub makeacr2acn {
#    system( "makeindex -s \"$_[0].ist\" -t \"$_[0].alg\" -o \"$_[0].acn\" \"$_[0].acr\"" );
#}

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

# add_cus_dep('uml', 'png', 0, 'uml2png');
# sub uml2png {
#     system("java -cp $PLANTUML_JAR net.sourceforge.plantuml.Run -failfast2 -nbtread auto -charset UTF-8 -o . -v -tpng \"$_[0].uml\" \"$_[0].png\"");
#     system("java -Djava.awt.headless=true -jar $PLANTUML_JAR -failfast2 -nbtread auto -charset UTF-8 -tlatex:nopreamble \"$_[0].uml\" \"$_[0].png\"");
# }

# add_cus_dep('uml', 'latex', 0, 'txt2latex');
# sub txt2latex {
#     # system("java -cp $PLANTUML_JAR net.sourceforge.plantuml.Run -failfast2 -nbtread auto -charset UTF-8 -o . -v -tpng \"$_[0].uml\" \"$_[0].png\"");
#     system("java -Djava.awt.headless=true -jar $PLANTUML_JAR -failfast2 -nbtread auto -charset UTF-8 -tlatex:nopreamble \"$_[0].tex\" \"$_[0].latex\"");
# }
#
# add_cus_dep('puml', 'tex', 0, 'puml2tex');
# sub puml2latex {
#     # system("java -cp $PLANTUML_JAR net.sourceforge.plantuml.Run -failfast2 -nbtread auto -charset UTF-8 -o . -v -tpng \"$_[0].uml\" \"$_[0].png\"");
#     system("java -Djava.awt.headless=true -jar $PLANTUML_JAR -failfast2 -nbtread auto -charset UTF-8 -o . -v -tlatex:nopreamble \"$_[0].puml\"");
# }


# A standard method of using it is with the asymptote LaTeX style file
# (http://mirror.ctan.org/graphics/asymptote/doc/asymptote.sty)
# The graphics drawing code is in the tex file, and applying pdflatex to
# the tex file produces one or more files with a base name the same as
# or related to the main tex file, but with the extension 'asy'.  The
# .asy is processed by the program asy (part of the asymptote
# software) to produce graphics files (which may be eps, tex, or pdf
# files) that are used the next time pdflatex is run on the main tex
# file.
#
# Latexmk can be arranged to run asymptote (i.e., the program asy)
# when needed, by defining the following custom dependency.  (The code
# is to be put in one of latexmk's rc files, e.g., ~/.latexmkrc.)
#

## OLD simple method (taken from the documentation for V. 2.03 of
## asymptote).  These definitions are simple, but they may not always
## give the desired type of output file, and they do not ensure that
## latexmk has dependency information about files imported from the
## asy file.
#OLD sub asy {return system("asy \"$_[0]\"");}
#OLD add_cus_dep("asy","eps",0,"asy");
#OLD add_cus_dep("asy","pdf",0,"asy");
#OLD add_cus_dep("asy","tex",0,"asy");


# The following definitions arrange to run asy with the correct output
# file type.  They run asy in a verbose mode so that dependency
# information on imported files can be extracted.  To avoid adding a
# lot of extra printout on the screen of unimportant messages, the
# output is sent to a log file.  Since this includes error messages,
# which the user should see, latexmk types out error messages and the
# like. These definitions need latexmk 4.48 or later.

add_cus_dep("puml","eps",0,"puml2eps");
add_cus_dep("puml","pdf",0,"puml2pdf");
add_cus_dep("puml","png",0,"puml2png");
add_cus_dep("puml","latex",0,"puml2latex");

# sub puml2png { return puml2x( $_[0], 'png' ); }
# sub puml2eps { return puml2x( $_[0], 'eps' ); }
# sub puml2pdf { return puml2pd( $_[0], 'pdf' ); }
# sub puml2latex { return puml2x( $_[0], 'latex' ); }

sub puml2pdf {
   system("plantuml -failfast2 -nbtread auto -charset UTF-8 -tpdf '$_[0].puml' >& '$_[0].log'");
}

sub puml2latex {
   system("plantuml -failfast2 -nbtread auto -charset UTF-8 -tlatex:nopreamble '$_[0].puml' >& '$_[0].log'");
}

sub puml2eps {
   system("plantuml -failfast2 -nbtread auto -charset UTF-8 -teps '$_[0].puml' >& '$_[0].log'");
}

sub puml2png {
   system("plantuml -failfast2 -nbtread auto -charset UTF-8 -tpng '$_[0].puml' >& '$_[0].log'");
}

sub puml2x {
   warn "==Zero $_[0]";
   warn "==One  $_[1]";
   my $ret = system("plantuml -v -failfast2 -nbtread auto -charset UTF-8 -tlatex:nopreamble '$_[1]' '$_[0]'");
   my $FH = new FileHandle;
   open $FH, "$_[0].log";
   %imp = ();

   while (<$FH>) {
     if (/^(Including|Loading) .* from (.*)\s*$/) {
          my $import = $2;
	  $imp{$import} = 1;
       }
       elsif ( /^error/ || /^.*\.asy: \d/ ) {
           warn "==Message from asy: $_";
	   $ret = 1;
       }
       elsif ( /^kpsewhich / || /^Processing / || /^Using /
               || /^Welcome / || /^Wrote /|| /^cd /|| /^gs /
	     ) {
       }
       else {
           warn "==Message from plantuml: $_";
       }
   }
   close $FH;
# For latexmk 4.48
   rdb_set_source( $rule, keys %imp );
   return $ret;
}



add_cus_dep('gif', 'eps', 0, 'gif2eps');
sub gif2eps {
	return system("convert \"$_[0].gif\" \"$_[0].eps\"");
}

add_cus_dep('jpg', 'eps', 0, 'jpg2eps');
sub jpg2eps {
	return system("convert \"$_[0].jpg\" \"$_[0].eps\"");
}

add_cus_dep('png', 'eps', 0, 'png2eps');
sub png2eps {
	return system("convert \"$_[0].png\" \"$_[0].eps\"");
}

add_cus_dep('svg', 'eps', 0, 'svg2eps');
sub svg2eps {
  system("/Applications/Inkscape.app/Contents/MacOS/inkscape --export-area-drawing --export-text-to-path --export-type='eps' '$_[0].svg'");
}

add_cus_dep('svg', 'pdf_tex', 0, 'svg2pdf_tex');
sub svg2pdf_tex {
 return system("/Applications/Inkscape.app/Contents/MacOS/inkscape --export-area-drawing --export-type='pdf' --export-latex '$_[0].svg'");
}

add_cus_dep('svg', 'eps_tex', 0, 'svg2eps_tex');
sub svg2eps_tex {
  system("/Applications/Inkscape.app/Contents/MacOS/inkscape --export-area-drawing --export-type='eps' --export-latex '$_[0].svg'");
}
