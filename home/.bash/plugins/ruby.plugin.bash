# ruby and rubygems specific functions and settings

# Make commands installed with 'gem install --user-install' available
# ~/.gem/ruby/${RUBY_VERSION}/bin/

if hash ruby 2>/dev/null; then
    if hash gem 2>/dev/null; then
        path_munge "$(ruby -e 'print Gem.user_dir')/bin" after
    fi
fi

# removes installed gem
# param '1: installed gem name'
function gem-remove () {
  gem list | grep $1 | awk '{ print $1; }' | xargs sudo gem uninstall
}
