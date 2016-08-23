# Use xelatex instead of pdflatex
# $pdflatex = 'xelatex -synctex=1 -interaction=nonstopmode --shell-escape %O %S';
$pdflatex = "pdflatex -interaction=nonstopmode -synctex=1 %O %S";

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

# make latexmk run makeglossaries automatically
add_cus_dep('glo', 'gls', 0, 'makeglossaries');
add_cus_dep('acn', 'acr', 0, 'makeglossaries');

sub makeglossaries {
    system "makeglossaries $_[0]";
    if ( -z "$_[0].glo" ) {
        open GLS, ">$_[0].gls";
        close GLS;
    }
    return 0;
}
