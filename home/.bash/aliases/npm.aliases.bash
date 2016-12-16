# Some aliases for npm

# alias nup='for package in $(npm -g outdated --parseable --depth=0 | cut -d: -f2); do IFS='@' read -ra splits <<< "${package}" && if [ "${splits[0]}" != "npm" ]; then npm -g install "$package"; fi done'

nup() { for package in $(\ls -F "${NPM_CONFIG_PREFIX}/lib/node_modules" | sed -n 's/[^@]$//p' | xargs npm outdated --no-color -pg --depth=0 | awk '{gsub(/\/.*\//,"",$1); print}' | \grep -o ":.*:" | sed 's/.$//; s/^.//'); do IFS=@ read -ra splits <<< "${package}" && if [ "${splits[0]}" != "npm" ]; then npm install -g "${package}"; fi done }
