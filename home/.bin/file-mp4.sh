#!/bin/bash

# Static variables
readonly VERSION="2.1.3"
readonly PROG=$(basename $0)

# Setup environment
TMP='/tmp'
SHOW=''
SEASON=''
EPISODE=''
TITLE=''
FILES=()
# COLOR='\E[33;40m\033[1m' #comment these out if your term doesn't support colors
COLORWARN='\E[31;40m\033[1m'

# Function to display colorized output
function cecho () {
	MESSAGE=${1:-"Error: No message passed"}
	echo -e "${COLOR}${MESSAGE}"
	tput sgr0
}

# Function to display colorized warnings
function cwarn () {
	MESSAGE=${1:-"Error: No message passed"}
	echo -e "${COLORWARN}${MESSAGE}"
	tput sgr0
}

# Function to determine if variable is an integer
function is_int() {
	return $(test "$1" -eq "$1" > /dev/null 2>&1);
}

# Function to verify that necessary support binaries exist
function bincheck() {
	MISSING=''
	case $EXT in
		"m4v")
			MP4INFO=$(which mp4info 2>/dev/null)
			[[ ! -e "$MP4INFO" ]] && MISSING+="mp4info (optional with '-n'), "
		;;
		"mp4")
			MP4INFO=$(which mp4info 2>/dev/null)
			[[ ! -e "$MP4INFO" ]] && MISSING+="mp4info (optional with '-n'), "
		;;
	esac
	if [ -n "$MISSING" ]; then
		echo "Error: cannot find the following binaries: ${MISSING%%, }"
		exit
	fi
}

# Function to parse mp4info output to find tags and convert to VORBISCOMMENT
function mp4tags() {
	TAGS2=${TAGS}.alac
	$SED -i "/ \w*: /w${TAGS2}" "$TAGS"
	$SED -i "s/^ //" "$TAGS2"
	$SED -i "s/: /=/" "$TAGS2"
	$SED -i "s/ of [0-9]\+//" "$TAGS2"
	$SED -i "s/\(.*\)=/\U\1=/" "$TAGS2"
	$SED -i "s/TV EPISODE NUMBER=/EPISODE NUMBER=/" "$TAGS2"
	mv "$TAGS2" "$TAGS"
}

# Function to copy tags for supported formats
function processtags() {
	OUTPUT="\nProcessing tags for '$FILE'..."
	TAGS=$TMP/$PROG.$RANDOM.tags
	if [ "$EXT" == "mp4" ]; then
		$MP4INFO "${FILE}" >"${TAGS}"
		mp4tags
	elif [ "$EXT" == "m4v" ]; then
        $MP4INFO "$FILE" >"${TAGS}"
		mp4tags
	else
		OUTPUT+="  tags not supported by for this format\n"
		return
	fi
	if [[ $? -ne 0 || ! -s "$TAGS" ]]; then
		OUTPUT+="\nWarning: tags could not be read from \"$FILE\"\n"
        # rm "$FILE"
	else

        SHOW=$(awk -F "=" '/TV SHOW/ {print $2}' "$TAGS")
        SEASON=$(awk -F "=" '/TV SEASON/ {print $2}' "$TAGS")
        SEASON=$(printf %02d "$SEASON")
        EPISODE=$(awk -F "=" '/TV EPISODE/ {print $2}' "$TAGS")
        EPISODE=$(printf %02d "$EPISODE")
        TITLE=$(awk -F "=" '/NAME/ {print $2}' "$TAGS")
		# OUTPUT+="\nShow: \"$SHOW\""
		# OUTPUT+="\nSeason: \"$SEASON\""
        # OUTPUT+="\nEpisode: \"$EPISODE\""
		# OUTPUT+="\nTitle: \"$TITLE\""


        if [ -z "$SHOW" ]; then
            OUTPUT+="\nWarning: file does not contain show tags \"$FILE\"\n"
            # rm "$FILE"
        else
            local SHOWDIR="${SHOW}/S${SEASON}"
            mkdir -p "$SHOWDIR"

            local DESTNAME="${SHOW} - S${SEASON}E${EPISODE} - ${TITLE}.${EXT}"
            # DESTNAME=$(printf -v name '%q' "$DESTNAME")
            DESTNAME=$(gsed 's/[\/&]/-/g' <<< "$DESTNAME")
            OUTPUT+="\nDestination: \"$DESTNAME\""
            local DEST="${SHOWDIR}/$DESTNAME"
            # DEST=$(printf '%q' "$DEST")

            if [ ! -f "$DEST" ]; then
                mv "$FILE" "$DEST" > /dev/null 2>&1
                # cp "$FILE" "$DEST" > /dev/null 2>&1
                if [[ $? -ne 0 ]]; then
                    OUTPUT+="\nWarning: file could not be copied to \"$DEST\"\n"
                else
                    # mv "$FILE" ./trash/
                    OUTPUT+="  complete\n"
                fi
            else
                OUTPUT+="  destination exists\n"
            fi
        fi
	fi

	rm "$TAGS"
	echo -ne "$OUTPUT"
}

# Process arguments
while [ $# -ne 0 ]; do
	# Match known arguments
	if [ "$1" == "-V" ]; then
		echo "Version $VERSION"
		exit
	# Anything that's not a known argument gets treated as a file
	else
		FILES[${#FILES[*]}]=$1
	fi
	shift
done

# Define and verify core apps exist
SED=$(which gsed 2>/dev/null)
TPUT=$(which trash 2>/dev/null)
MISSING=''
[ ! -e "$SED" ] && MISSING+='sed, '
[ ! -e "$TPUT" ] && MISSING+='trash-put, '
if [ -n "$MISSING" ]; then
	echo "Error: cannot find the following binaries: ${MISSING%%, }"
	exit
fi


# Process each passed file sequentially
for FILE in "${FILES[@]}"; do
	# Verify file exists
	if [ ! -e "$FILE" ]; then
		echo "Error: '$FILE' does not exist"
		exit 1
	fi

	# Determine file type and base filename
	# NAME="${FILE%.*}"
	EXT=$(echo "${FILE##*.}" | $SED 's/\(.*\)/\L\1/')

	# Exit if wrong file passed
	if [[ "$EXT" != "mp4" && "$EXT" != "m4v" ]]; then
		echo "Error: '$FILE' is not a supported input format"
		exit 1
	fi

	# Verify support binaries
	bincheck

	# Transcode file, concurrently up to number of specified threads
	cecho "\nProcessing '$FILE'...\n"

    processtags

	# if [ $(jobs | wc -l) -lt $THREADS ]; then
	# 	transcode &
	# fi
	# while [ $(jobs | wc -l) -ge $THREADS ]; do
	# 	sleep 0.1
	# 	jobs >/dev/null
	# done
done

# Wait for any remaining processes to finish before exiting
wait
