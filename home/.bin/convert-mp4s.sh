#!/bin/bash
function walk_tree {
      local directory="$1"
      local filename
      local base
      local ext
      local of
      local i
      for i in "$directory"/*;
      do
      echo "File: $i"
      if [ "$i" = . -o "$i" = .. ]; then
            continue
        elif [ -d "$i" ]; then  # Process directory and / or walk-down into directory
            echo "Directory: $i"
            cd "$i"

            for f in *;
            do
                filename="${f##*/}"
                base="${f%.[^.]*}"
                ext="${f:${#base} + 1}"
                if [[ -z "$base" && -n "$ext" ]]; then
                    base=".$ext"
                    ext=""
                fi

                of="`basename "$filename" | sed 's/\(.*\)\..*/\1/'`.m4v"
                ts HandBrakeCLI -Z "Apple 1080p60 Surround" -m -i "$f" -o "${HOME}/Movies/after/$of" > /dev/null
                ts -dn mv "${HOME}/Movies/after/$of" "${HOME}/Documents/Videos/$of" > /dev/null
                ts -dn trash "$f" > /dev/null
            done

            cd ..

            # add command here to process all files in directory (i.e. ls -l "$i/"*)
            walk_tree "$i"      # DO NOT COMMENT OUT THIS LINE!!
        else
            filename="${i##*/}"
            base="${i%.[^.]*}"
            ext="${i:${#base} + 1}"
            if [[ -z "$base" && -n "$ext" ]]; then
                base=".$ext"
                ext=""
            fi

            of="`basename "$filename" | sed 's/\(.*\)\..*/\1/'`.m4v"
            ts HandBrakeCLI -Z "Apple 1080p60 Surround" -m -i "$i" -o "${HOME}/Movies/after/$of" > /dev/null
            ts -dn mv "${HOME}/Movies/after/$of" "${HOME}/Documents/Videos/$of" > /dev/null
            ts -dn trash "$i" > /dev/null
        fi
      done
}

walk_tree $1
