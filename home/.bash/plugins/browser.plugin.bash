# render commandline output in your browser
# based on https://gist.github.com/318247

# pipe html to a browser
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

# write wget into a temp file and pump it into your browser
# example '$ browser-raw google.com'
function browser-raw() {
  if [ -t 0 ]; then
    if [ -n "$1" ]; then
      wget -qO- $1 | browser
    else
      reference browser-raw
    fi
  fi
}
