#!/bin/bash

# Static variables
readonly VERSION="1.0.0"
readonly PROG=$(basename "$0")

shopt -s nullglob

TMP='/tmp'
CUM_DUR=0
CH_DURS=()
CH_NAMES=()
CH_FILES=()

# Use BASEDIR from commandline, or current dir if none given
CURRENTDIR="$(pwd -P)"
BASEDIR=${1:-$CURRENTDIR}

MP4INFO=$(which mp4info 2>/dev/null)
MP4BOX=$(which MP4Box 2>/dev/null)
SED=$(which gsed 2>/dev/null)

function mp4boxduration() {
  local hrs
  local min
  local sec
  local msec

  hrs=$($SED -En 's/Computed Duration ([0-9]{2}):([0-9]{2}):([0-9]{2}).([0-9]{3}).*/\1/p' "${TAGS}")
  # delete BOTH leading and trailing whitespace from each line
  hrs=$(echo "$hrs" | $SED -E 's/^[ \t]*//;s/[ \t]*$//')

  min=$($SED -En 's/Computed Duration ([0-9]{2}):([0-9]{2}):([0-9]{2}).([0-9]{3}).*/\2/p' "${TAGS}")
  min=$(echo "$min" | $SED -E 's/^[ \t]*//;s/[ \t]*$//')

  sec=$($SED -En 's/Computed Duration ([0-9]{2}):([0-9]{2}):([0-9]{2}).([0-9]{3}).*/\3/p' "${TAGS}")
  sec=$(echo "$sec" | $SED -E 's/^[ \t]*//;s/[ \t]*$//')

  mseC=$($SED -En 's/Computed Duration ([0-9]{2}):([0-9]{2}):([0-9]{2}).([0-9]{3}).*/\4/p' "${TAGS}")
  mseC=$(echo "$mseu" | $SED -E 's/^[ \t]*//;s/[ \t]*$//')

  echo "$((msec + (sec * 1000) + (min * 1000 * 60) + (hrs * 1000 * 60 * 60)))"
}

function mp4boxsize() {
  local width
  local height

  width=$($SED -En 's/AVC\/H264 Video - Visual Size ([0-9]{3,4}) x ([0-9]{3,4}).*/\1/p' "${TAGS}")
  # delete BOTH leading and trailing whitespace from each line
  width=$(echo "$width" | $SED -E 's/^[ \t]*//;s/[ \t]*$//')

  height=$($SED -En 's/AVC\/H264 Video - Visual Size ([0-9]{3,4}) x ([0-9]{3,4}).*/\2/p' "${TAGS}")
  height=$(echo "$height" | $SED -E 's/^[ \t]*//;s/[ \t]*$//')

  echo "$width $height"
}

# Creates a nice format of a datetime.timedelta structure, including milliseconds
function formatTimedelta() {
  local td="$1"
  local hrs
  local min
  local sec
  local msec

  msecs="$(((td % 1000)))"
  secs="$(((td / 1000) % 60))"
  mins="$((((td / (1000*60)) % 60)))"
  hrs="$((((td / (1000*60*60)) % 24)))"

  printf "%02d:%02d:%02d.%03d" $hrs $mins $secs $msecs
}

# Function to copy tags for supported formats
function processtags() {
  TAGS=$TMP/$PROG.$RANDOM.tags
  local FILE="$1"
  local dur
  local wh
  local timecode

  xbase=${FILE##*/}
  xpref=${xbase%.*}

  # append chapter name
  CH_NAMES=("${CH_NAMES[@]}" "${xpref}")

  ( $MP4BOX -info -quiet -std "${FILE}" > >(tee "${TAGS}") 2> >(tee "${TAGS}" >&2) ) &>/dev/null
  dur=$(mp4boxduration)
  echo $"dur: $dur"
  wh=$(mp4boxsize)
  echo $"wh: $wh"

  timecode=$(formatTimedelta "${CUM_DUR}")
  # append chapter timecode
  CH_DURS=("${CH_DURS[@]}" "${timecode}")

  # Count the cumulative duration
  CUM_DUR=$((CUM_DUR + dur))

  # TITLE=$(awk -F "=" '/NAME/ {print $2}' "$TAGS")

  rm "$TAGS"
}

# Saves a list of chapter information to a chapter file in the common chapter syntax
function savechaptersfile() {
  CHAPTERS_FILE=$TMP/$PROG.$RANDOM.txt
  touch "${CHAPTERS_FILE}"

  # Writing the common CHAPTER syntax
  #
  # CHAPTERX=h:m:s[:ms or .ms] on one line and,
  # CHAPTERXNAME=name on the other
  #
  # The order is not important but chapter lines MUST be declared sequentially
  # (same X value expected for 2 consecutive lines).

  local length=${#CH_DURS[@]}   # get length of an array

  # Use for loop to read all values and indexes
  for (( i=0; i<${length}; i++ ));
  do
    printf "CHAPTER%02d=%s\\n" $((i + 1)) "${CH_DURS[$i]}" >> "${CHAPTERS_FILE}"
    printf "CHAPTER%02dNAME=%s\\n" $((i + 1)) "${CH_NAMES[$i]}" >> "${CHAPTERS_FILE}"
  done

  echo "${CHAPTERS_FILE}"
}


function walk_tree {
  # echo "Directory: $1"
  local directory="$1"
  local timecode
  local chapters
  local final
  local i

  for i in "${directory}"/*;
  do
    # echo "File: $i"
    if [ "$i" = . -o "$i" = .. ]; then
      continue
    elif [ -d "$i" ]; then  # Process directory and / or walk-down into directory
      # echo "Directory: $i"
      cd "$i"

      # reset
      final=""
      CUM_DUR=0
      CH_DURS=()
      CH_NAMES=()
      CH_FILES=()

      for l in *.mp4 *.m4v;
      do
        processtags "${l}";
        final+=" -cat \"$l\"";
      done;

      # After all files, add the final chapter as the end for this segment
      timecode=$(formatTimedelta "${CUM_DUR}")
      CH_NAMES=("${CH_NAMES[@]}" End)
      CH_DURS=("${CH_DURS[@]}" "${timecode}")

      # Write the chapters file to out
      chapters=$(savechaptersfile)
      # Add the chapter file
      # final+=" -chap \"${chapters}\"";

      # echo $final
      if [ -n "$final" ]; then
        cmd="MP4Box -quiet $final -new \"../${PWD##*/}.m4v\""
        eval "$cmd"

        # cmd="MP4Box -quiet -add \"${chapters}:chap\" \"../${PWD##*/}.mp4\""
        cmd="MP4Box -chap \"${chapters}\" \"../${PWD##*/}.m4v\""
        eval "$cmd"
      fi

      rm "${chapters}"

      cd ..

      # add command here to process all files in directory (i.e. ls -l "$i/"*)
      walk_tree "$i"      # DO NOT COMMENT OUT THIS LINE!!
    else
      continue    # replace continue to process individual file (i.e. echo "$i")
    fi
  done
}

function run() {
  # find all files
  walk_tree "${BASEDIR}"

}

run

# TODO: Generate videosize. The desired maximum w/h size for the output video,
# default is 1024:576 (in case of multiple sizes for videos then all videos
# above this size are downsized to match) Aspect ratios will be downscaled as
# needed.

# final="";for i in *.mp4; do final+=" -cat \"$i\""; done; echo "MP4Box $final -new \"../${PWD##*/}.mp4\"" | pbcopy

# MP4Box  -cat "*.mp4" -new "../BIWS Premium - Financial Modeling Fundamentals.mp4"

# MP4Box  -cat "01-06-Interview-Model-Overview.mp4" -cat "01-07-Walk-Through-Statements.mp4" -cat "01-08-Depreciation-Changes.mp4" -cat "01-09-Inventory-Changes-Cash-Debt.mp4" -cat "01-10-Inventory-LIFO-vs-FIFO.mp4" -cat "01-11-Accrued-Expenses.mp4" -cat "01-12-Deferred-Revenue.mp4" -cat "01-13-Deferred-Income-Taxes.mp4" -cat "01-14-Dividends.mp4" -cat "01-15-Issue-Repurchase-Shares.mp4" -cat "01-16-Raising-Paying-Off-Debt.mp4" -cat "01-17-Bailout.mp4" -cat "01-18-Goodwill-Impairment.mp4" -cat "01-19-Writing-Down-Debt.mp4" -cat "01-20-iPhone-Factories-Debt-Part-1.mp4" -cat "01-21-iPhone-Factories-Debt-Part-2.mp4" -cat "01-22-iPhone-Factories-Debt-Part-3.mp4" -cat "01-23-Making-iPhones-Part-1.mp4" -cat "01-24-Making-iPhones-Part-2.mp4" -cat "10-01-Income-Statement.mp4" -cat "10-02-Balance-Sheet-Assets.mp4" -cat "10-03-Balance-Sheet-Liabilities-SE.mp4" -cat "10-04-Cash-Flow-Statement.mp4" -cat "10-05-Linking-Statements.mp4" -new "../01 Accounting Fundamentals.mp4"
