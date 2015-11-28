# Some aliases for Homebrew Dupes
cite 'about-alias'
about-alias 'homebrew dupes abbreviations'

if [ $(uname) = "Darwin" ]; then
    if hash brew 2>/dev/null; then
        if hash ggrep 2>/dev/null; then
            alias grep='ggrep --color'
        fi
    fi
fi

