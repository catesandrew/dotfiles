# based on https://gist.github.com/318247

# about-plugin 'render commandline output in your browser'

# about 'pipe html to a browser'
# example '$ echo "<h1>hi mom!</h1>" | browser'
# example '$ ron -5 man/rip.5.ron | browser'
function browser() {
    if [ -t 0 ]; then
        if [ -n "$1" ]; then
            open $1
        else
            reference browser
        fi

    else
        f="/tmp/browser.$RANDOM.html"
        cat /dev/stdin > $f
        open $f
    fi
}

# about 'write wget into a temp file and pump it into your browser'
# example '$ raw google.com'
function raw() {
    if [ -t 0 ]; then
        if [ -n "$1" ]; then
            wget -qO- $1 | browser
        else
            reference raw
        fi
    fi
}
