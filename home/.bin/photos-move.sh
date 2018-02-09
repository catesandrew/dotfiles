#!/bin/bash

# Reads EXIF creation date from all .JPG files in the
# current directory and moves them carefully under
#
#   $BASEDIR/YYYY/YYYY-MM/YYYY-MM-DD/
#
# ...where 'carefully' means that it does not overwrite
# differing files if they already exist and will not delete
# the original file if copying fails for some reason.
#
# It DOES overwrite identical files in the destination directory
# with the ones in current, however.
#
# This script was originally written and put into
# Public Domain by Jarno Elonen <elonen@iki.fi> in June 2003.
# Feel free to do whatever you like with it.

# Defaults
TOOLS=(exiftool jq trash rsync) # Also change settings below if changing this, the output should be in the format YYYY:MM:DD
DEFAULTDIR="$HOME/ownCloud/Photos"
MAXDEPTH="9"
#MAXDEPTH=''
# activate debugging from here
#set -o xtrace
#set -o verbose

# Improve error handling
set -o errexit
set -o pipefail

# Check whether needed programs are installed
for TOOL in ${TOOLS[*]}
do
    hash $TOOL 2>/dev/null || { echo >&2 "I require $TOOL but it's not installed.  Aborting."; exit 1; }
done

# Enable handling of filenames with spaces:
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

# Use BASEDIR from commandline, or default if none given
BASEDIR=${1:-$DEFAULTDIR}

function phashconvert {
  local infile="$1"

  # syntax: phashconvert imagefile

  # get 42 values from each image and put into arrays
  # first: grep verbose info to get all lines that include PH
  # second: sed returns the pairs of comma separated values
  # third: sed removes all spaces
  # fourth: tr converts comma to new line so all values are on their own line
  # then convert list of values to arr

  arr1=(`identify -quiet -verbose -moments -alpha off "$infile[0]" | grep "PH[1-7]" | sed -n 's/.*: \(.*\)$/\1/p' | sed 's/ *//g' | tr "," "\n"`)
  num1="${#arr1[*]}"

  # test for correct number of values
  if [ $num1 -ne 42 ]; then
    echo "--- Number of Phash Values ($num1) is Incorrect --- "
    exit 1
  fi

  # convert 42 values into string of 168 digits as hash using 4 digits for each float
  hash1=""
  for ((i=0; i<42; i++)); do
    # bc cannot handle scientific notation; have to change e to 10^
    val1=`echo "${arr1[$i]}" |\
      awk ' { printf "%0004d", ($1<0)?100*($1-0.005):100*($1+0.005) } '`
    hash1="${hash1}${val1}"
  done

  echo "$hash1"
}

for FILE in $(find "$(pwd -P)" -maxdepth "$MAXDEPTH" -not -wholename "*._*" -iname "*.JPG" -or -iname "*.JPEG" -or -iname "*.GIF" -or -iname "*.HEIC" -or -iname "*.CRW" -or -iname "*.THM" -or -iname "*.RW2" -or -iname '*.ARW' -or -iname "*AVI" -or -iname "*MOV" -or -iname "*MP4"  -or -iname "*MTS" -or -iname "*PNG" -and -not -iname "*_convertToPNG*")
do
	INPUT=${FILE}
	DATE=$(exiftool -quiet -tab -dateformat "%Y:%m:%d" -json -DateTimeOriginal "${INPUT}" | jq --raw-output '.[].DateTimeOriginal.val')
	if [ "$DATE" == "null" ]  # If exif extraction with DateTimeOriginal failed
	then
		DATE=$(exiftool -quiet -tab -dateformat "%Y:%m:%d" -json -MediaCreateDate "${INPUT}" | jq --raw-output '.[].MediaCreateDate.val')
	fi
	if [ -z "$DATE" ] || [ "$DATE" == "null" ] # If exif extraction failed
	then
		DATE=$(stat -f "%Sm" -t %F "${INPUT}" | awk '{print $1}'| sed 's/-/:/g')
	fi
	if [ ! -z "$DATE" ]; # Doublecheck
	then
		YEAR=$(echo $DATE | sed -E "s/([0-9]*):([0-9]*):([0-9]*)/\\1/")
		MONTH=$(echo $DATE | sed -E "s/([0-9]*):([0-9]*):([0-9]*)/\\2/")
		DAY=$(echo $DATE | sed -E "s/([0-9]*):([0-9]*):([0-9]*)/\\3/")
		if [ "$YEAR" -gt 0 ] & [ "$MONTH" -gt 0 ] & [ "$DAY" -gt 0 ]
		then
			# OUTPUT_DIRECTORY=${BASEDIR}/${YEAR}_${MONTH}_${DAY}
			OUTPUT_DIRECTORY=${BASEDIR}/${YEAR}/${MONTH}/${DAY}
			mkdir -pv ${OUTPUT_DIRECTORY}
			OUTPUT=${OUTPUT_DIRECTORY}/$(basename ${INPUT})
			if [ -e "$OUTPUT" ] && ! cmp -s "$INPUT" "$OUTPUT"
			then
				echo "WARNING: '$OUTPUT' exists already and is different from '$INPUT'."
                # input_hash=$(phashconvert "$INPUT")
                # output_hash=$(phashconvert "$OUTPUT")
                # echo "HASH: $input_hash"
                # echo "HASH: $output_hash"
                open "$INPUT"
                open "$OUTPUT"
                break;
			else
				echo "Moving '$INPUT' to $OUTPUT"
				rsync -ah --progress "$INPUT"  "$OUTPUT"
				if ! cmp -s "$INPUT" "$OUTPUT"
				then
					echo "WARNING: copying failed somehow, will not delete original '$INPUT'"
				else
					trash "$INPUT"
				fi
			fi
		else
		  echo "WARNING: '$INPUT' doesn't contain date."
		fi
	else
		echo "WARNING: '$INPUT' doesn't contain date."
	fi
done

# restore $IFS
IFS=$SAVEIFS
