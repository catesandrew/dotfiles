# Some aliases for npm

cite 'about-alias'
about-alias 'npm abbreviations'

# Use `nup` most of the time; use `nub` if you have packages that are newer
# than <package>@latest and you want to keep them on the absolute newest
# version rather than latest.

alias nub='for package in $(npm -g outdated --parseable --depth=0 | cut -d: -f3); do npm -g install "$package"; done'
alias nup='for package in $(npm -g outdated --parseable --depth=0 | cut -d: -f2); do npm -g install "$package"; done'
