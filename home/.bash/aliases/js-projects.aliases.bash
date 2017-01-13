# Some aliases for javascript projects

findjs() { find . -maxdepth 8 -type f -name "*.js" ! -path './node_modules*' ! -path './.git' | while IFS= read -r file; do grep -EHn "$@" "$file"; done }
