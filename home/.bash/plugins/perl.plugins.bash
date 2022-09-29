if brew_contains_element "perl"; then
  # echo "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=$HOME/perl5)"
  PERL_MB_OPT="--install_base \"$HOME/perl5\"";
  export PERL_MB_OPT;
  PERL_MM_OPT="INSTALL_BASE=$HOME/perl5";
  export PERL_MM_OPT;
fi
