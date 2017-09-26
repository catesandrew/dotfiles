#!/bin/sh
# converts HTML from a URL, file, or stdin to markdown
# uses an available program to fetch URL and tidy to normalize it first

REQUIRED="tidy"
SYNOPSIS="converts HTML from a URL, file, or STDIN to markdown-formatted text."

THIS=${0##*/}

NEWLINE='
'

err ()  { echo "$*"   | fold -s -w ${COLUMNS:-110} >&2; }
errn () { printf "$*" | fold -s -w ${COLUMNS:-110} >&2; }

usage () {
    err "$1 - $2" # short description
    err "See the $1(1) man page for usage."
}

# Portable which(1).
pathfind () {
    oldifs="$IFS"; IFS=':'
    for _p in $PATH; do
        if [ -x "$_p/$*" ] && [ -f "$_p/$*" ]; then
            IFS="$oldifs"
            return 0
        fi
    done
    IFS="$oldifs"
    return 1
}

for p in pandoc $REQUIRED; do
    pathfind $p || {
        err "You need '$p' to use this program!"
        exit 1
    }
done

CONF=$(pandoc --dump-args "$@" 2>&1) || {
    errcode=$?
    echo "$CONF" | sed -e '/^pandoc \[OPTIONS\] \[FILES\]/,$d' >&2
    [ $errcode -eq 2 ] && usage "$THIS" "$SYNOPSIS"
    exit $errcode
}

OUTPUT=$(echo "$CONF" | sed -ne '1p')
ARGS=$(echo "$CONF" | sed -e '1d')


grab_url_with () {
    url="${1:?internal error: grab_url_with: url required}"

    shift
    cmdline="$@"

    prog=
    prog_opts=
    if [ -n "$cmdline" ]; then
	eval "set -- $cmdline"
	prog=$1
	shift
	prog_opts="$@"
    fi

    if [ -z "$prog" ]; then
	# Locate a sensible web grabber (note the order).
	for p in phantomjs swget lynx w3m curl links w3c; do
		if pathfind $p; then
		    prog=$p
		    break
		fi
	done

	[ -n "$prog" ] || {
            errn "$THIS:  Couldn't find a program to fetch the file from URL "
	    err "(e.g. wget, w3m, lynx, w3c, or curl)."
	    return 1
	}
    else
	pathfind "$prog" || {
	    err "$THIS:  No such web grabber '$prog' found; aborting."
	    return 1
	}
    fi

    # Setup proper base options for known grabbers.
    base_opts=
    case "$prog" in

    phantomjs)
      # phantomjs save_page.js http://example.com > page.html
      grabber_js=$THIS_TEMPDIR/grabber.js
      # echo "var system = require('system'), page = require('webpage').create(); page.open(system.args[1], function() { console.log(page.content); phantom.exit(); });" >| "${grabber_js}"

# var page = require('webpage').create(),
#     system = require('system'),
#     url = system.args[1];
#
# function onPageReady() {
#     var htmlContent = page.evaluate(function () {
#         return document.documentElement.outerHTML;
#     });
#
#     console.log(htmlContent);
#     phantom.exit();
# }
#
# page.open(url, function (status) {
#     function checkReadyState() {
#         setTimeout(function () {
#             var readyState = page.evaluate(function () {
#                 return document.readyState;
#             });
#
#             if ('complete' === readyState) {
#                 onPageReady();
#             } else {
#                 checkReadyState();
#             }
#         });
#     }
#
#     checkReadyState();
# });

    # echo "var page = require('webpage').create(), system = require('system'), url = system.args[1];" >| "${grabber_js}"
    # echo "function onPageReady() { var htmlContent = page.evaluate(function () { return document.documentElement.outerHTML; }); console.log(htmlContent); phantom.exit(); }" >> "${grabber_js}"
    # echo "page.open(url, function (status) { function checkreadystate() { setTimeout(function () { var readystate = page.evaluate(function () { return document.readystate; }); if ('complete' === readystate) { onpageready(); } else { checkreadystate(); } }); } checkreadystate(); });" >> "${grabber_js}"

# var page = require('webpage').create(),
#     system = require('system'),
#     url = system.args[1];
#
# page.open(url, function (status) { if (status !== 'success') {
#     console.log('unable to load the address!');
#     phantom.exit();
#   } else {
#     window.settimeout(function () {
#       console.log(page.content);
#       phantom.exit();
#     }, 1000);
#   }
# });

    # echo "var page = require('webpage').create(), system = require('system'), url = system.args[1];" >| "${grabber_js}"
    # echo "function onPageReady() { var htmlContent = page.evaluate(function () { return document.documentElement.outerHTML; }); console.log(htmlContent); phantom.exit(); }" >> "${grabber_js}"
    # echo "page.open(url, function (status) { if (status !== 'success') { console.error('unable to load the address!'); phantom.exit(); } else { window.setTimeout(function () { onPageReady(); /*console.log(page.content); phantom.exit();*/ }, 1000); } });" >> "${grabber_js}"

# var page = require('webpage').create(),
#     system = require('system'),
#     url = system.args[1];
#
# var requestsArray = [];
#
# page.onResourceRequested = function(requestData, networkRequest) {
#   requestsArray.push(requestData.id);
# };
#
# page.onResourceReceived = function(response) {
#   var index = requestsArray.indexOf(response.id);
#   requestsArray.splice(index, 1);
# };
#
# page.open(url, function(status) {
#
#   var interval = setInterval(function () {
#     if (requestsArray.length === 0) {
#       clearInterval(interval);
#       var content = page.content;
#       console.log(content);
#       page.render('yourLoadedPage.png');
#       phantom.exit();
#     }
#   }, 500);
# });


    echo "var page = require('webpage').create(), system = require('system'), url = system.args[1];" >| "${grabber_js}"
    echo "page.settings.userAgent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2'" >> "${grabber_js}"
    echo "var requestsArray = [];" >> "${grabber_js}"
    echo "page.onResourceRequested = function(requestData, networkRequest) { requestsArray.push(requestData.id); };" >> "${grabber_js}"
    echo "page.onResourceReceived = function(response) { var index = requestsArray.indexOf(response.id); requestsArray.splice(index, 1); };" >> "${grabber_js}"
    echo "page.open(url, function(status) { var interval = setInterval(function () { if (requestsArray.length === 0) { clearInterval(interval); console.log(page.content); phantom.exit(); } }, 5000); });" >> "${grabber_js}"

      base_opts="${grabber_js}"
    ;;
    wget)  base_opts="-o-" ;;
    lynx)  base_opts="-source" ;;
    w3m)   base_opts="-dump_source" ;;
    curl)  base_opts="" ;;
    links) base_opts="-source" ;;
    w3c)   base_opts="-n -get" ;;
    *)     err "$THIS:  unhandled web grabber '$prog'; hope it succeeds."
    esac

    err "$THIS: invoking '$prog $base_opts $prog_opts $url'..."
    eval "set -- $base_opts $prog_opts"
    $prog "$@" "$url"
}

# Parse command-line arguments
parse_arguments () {
    while [ $# -gt 0 ]; do
        case "$1" in
            --encoding=*)
                wholeopt="$1"
                # extract encoding from after =
                encoding="${wholeopt#*=}" ;;
            -e|--encoding|-encoding)
                shift
                encoding="$1" ;; 
            --grabber=*)
                wholeopt="$1"
                # extract encoding from after =
                grabber="\"${wholeopt#*=}\"" ;;
            -g|--grabber|-grabber)
                shift
                grabber="$1" ;; 
            *)
                if [ -z "$argument" ]; then
                    argument="$1"
                else
                    err "Warning:  extra argument '$1' will be ignored."
                fi ;;
            esac
        shift
    done
}

argument=
encoding=
grabber=

oldifs="$IFS"
IFS=$NEWLINE
parse_arguments $ARGS
IFS="$oldifs"

inurl=
if [ -n "$argument" ] && ! [ -f "$argument" ]; then
    # Treat given argument as an URL.
    inurl="$argument"
fi

# As a security measure refuse to proceed if mktemp is not available.
pathfind mktemp || { err "Couldn't find 'mktemp'; aborting."; exit 1;  }

# Avoid issues with /tmp directory on Windows/Cygwin 
cygwin=
cygwin=$(uname | sed -ne '/^CYGWIN/p')
if [ -n "$cygwin" ]; then
    TMPDIR=.
    export TMPDIR
fi

THIS_TEMPDIR=
THIS_TEMPDIR="$(mktemp -d -t $THIS.XXXXXXXX)" || exit 1
readonly THIS_TEMPDIR

trap 'exitcode=$?
      [ -z "$THIS_TEMPDIR" ] || rm -rf "$THIS_TEMPDIR"
      exit $exitcode' 0 1 2 3 13 15

if [ -n "$inurl" ]; then
    err "Attempting to fetch file from '$inurl'..."

    grabber_out=$THIS_TEMPDIR/grabber.out
    grabber_log=$THIS_TEMPDIR/grabber.log
    if ! grab_url_with "$inurl" "$grabber" 1>$grabber_out 2>$grabber_log; then
        errn "grab_url_with failed"
        if [ -f $grabber_log ]; then
            err " with the following error log."
            err
            cat >&2 $grabber_log
        else
            err .
        fi
        exit 1
    fi

    argument="$grabber_out"
fi

if [ -z "$encoding" ] && [ "x$argument" != "x" ]; then
    # Try to determine character encoding if not specified
    # and input is not STDIN.
    encoding=$(
        head "$argument" |
        LC_ALL=C tr 'A-Z' 'a-z' |
        sed -ne '/<meta .*content-type.*charset=/ {
            s/.*charset=["'\'']*\([-a-zA-Z0-9]*\).*["'\'']*/\1/p
        }'
    )
fi

if [ -n "$encoding" ] && pathfind iconv; then
    alias to_utf8='iconv -f "$encoding" -t utf-8'
else # assume UTF-8
    alias to_utf8='cat'
fi 

htmlinput=$THIS_TEMPDIR/htmlinput

if [ -z "$argument" ]; then
    to_utf8 > $htmlinput                # read from STDIN
elif [ -f "$argument" ]; then
    # purge line (>| for noclobber)
    to_utf8 "$argument" >| $htmlinput    # read from file
else
    err "File '$argument' not found."
    exit 1
fi


# extract the protocol
PROTO="$(echo $1 | grep :// | sed -e's,^\(.*://\).*,\1,g')"
# remove the protocol
URL="$(echo ${1/$PROTO/})"
# extract the user (if any)
USER="$(echo $URL | grep @ | cut -d@ -f1)"
# extract the host
HOST="$(echo ${URL/$USER@/} | cut -d/ -f1)"
# extract the path (if any)
URL_PATH="$(echo $URL | grep / | cut -d/ -f2-)"

#echo "url: $URL"
#echo "  proto: $PROTO"
#echo "  user: $USER"
#echo "  host: $HOST"
#echo "  path: $PATH"

URL_PATH=$(echo "$URL_PATH" | awk '{ sub(/\/$/, "", $1) } 1')
FILE=/$URL_PATH
HTML_FILE=${FILE##/*/}
#echo ${FILE#/*/}  # ==> user/src/prog.c
#echo ${FILE##/*/} # ==> prog.c
#echo ${FILE%/*}   # ==> /home/user/src
#echo ${FILE%%/*}  # ==> nil
#echo ${FILE%.html}   # ==> /home/user/src/prog

# read2text ${1} > test.md
# cat test.md | gfm > 
# rm test.md

cat $htmlinput | html2text --unicode-snob --body-width=0 --dash-unordered-list >| "./${HTML_FILE}.md"
# if ! cat $htmlinput | pandoc --ignore-args -r html -w markdown "$@" ; then
#     err "Failed to parse HTML.  Trying again with tidy..."
#     tidy -q -asxhtml -utf8 $htmlinput | \
#         pandoc --ignore-args -r html -w markdown "$@"
# fi

# encode images
# mkdir images_temp
#
# for i in `sed -n '/<img/s/.*src="\([^"]*\)".*/\1/p' ${HTML_FILE}`;
#
#     do echo "######### download the image: ${i}";
#     #wget -q -P images_temp/ $i;
#     pushd "images_temp" > /dev/null
#     curl -O ${i}
#     popd > /dev/null
#
#     filename="${i##*/}"
#     #dir="images_temp/${i:0:${#i} - ${#i}}"
#     base="${i%.[^.]*}"
#     ext="${i:${#base} + 1}"
#
#     #echo "######### convert the image for size saving";
#     #convert -quality 70 `echo ${i##*/}` `echo ${i##*/}`.temp;
#
#     #echo "######### rename temp image";
#     #rm `echo ${i##*/}` && mv `echo ${i##*/}`.temp `echo ${i##*/}`;
#
#     echo "######### encode in base64";
#     k="`echo "data:image/${ext};base64,"`$(base64 "images_temp/${filename}")";
#
#     rm images_temp/*;
#
#     echo "######### replace string in html";
#     echo "s|$i|$k|" > foo.sed
#     sed -f foo.sed "${HTML_FILE}" > temp.html
#
#     echo "######### replace final file";
#     rm -rf $1 && mv temp.html ${HTML_FILE};
#     rm foo.sed
# done;
