# exiftool

> Read and write meta information in files.

- Remove all EXIF metadata from the given files:

`exiftool -All= {{file}}`

- Increase time photo taken by 1 hour in directory:

`exiftool "-AllDates+=0:0:0 1:0:0" {{directory}}`

- Decrease time photo taken by 1 day and 2 hours on JPEGs only:

`exiftool "-AllDates-=0:0:1 2:0:0" -ext jpg`

- Change only DateTimeOriginal by -1.5 hours & do not keep backups:

`exiftool -DateTimeOriginal-=1.5 -overwrite_original`

- Rename all JPEGs according to a DateTimeOriginal recursively:

`exiftool '-filename<DateTimeOriginal' -d %Y-%m-%d_%H-%M-%S%%lc.%%e {{directory}} -r -ext jpg`

## Searching for Files

- Find images in a directory that don't have a DateTimeOriginal

`exiftool -filename -filemodifydate -createdate -r -if '(not $datetimeoriginal) and $filetype eq "JPEG"' .`

- Output photos that don't have datetimeoriginal to a CSV
    - You'll need to set your Dropbox folder path. I have mine set as a global variable
    OLDFILE="$DROPBOX_PERSONAL/nodates.csv"

```
FILECOUNT=$(mdfind -count -onlyin "$DROPBOX_PERSONAL" 'kMDItemKind =="JPEG image"')
while IFS= read -r -d '' n; do
    FILECOUNT=$(( $FILECOUNT - 1 ))
    if grep -q "$n" "$OLDFILE"; then
        echo "Skipping $n"
        continue
    fi

    echo -ne "Remaining: $FILECOUNT\r"
    exiftool -q -if '(not $datetimeoriginal or ($datetimeoriginal eq "0000:00:00 00:00:00"))' -csv -common "$n" | sed 1d >> "$DROPBOX_PERSONAL/nodates.csv"
done < <( mdfind -onlyin "$DROPBOX_PERSONAL" 'kMDItemKind =="JPEG image"' -0 )
```

- See files File Modify Date recursively in a directory who don't have datetimeoriginal set

`exiftool -filemodifydate -r -if '(not $datetimeoriginal or ($datetimeoriginal eq "0000:00:00 00:00:00")) and ($filetype eq "JPEG")' .`

## Modifying Files

- Change JPG to jpg and MOV to mov in filenames

`for i in *.JPG; do mv "$i" "${i%%.JPG}.jpg"; done; !#:gs/JPG/MOV/:gs/jpg/mov/`

- Recursively

`find /path/to/directory -name *JPG -exec sh -c 'mv "$0" "${0%%.JPG}.jpg"; echo "Moved $0 to ${0%%.JPG}.jpg"' {} \;`

- Change last created and modified for files in a directory
    - The date syntax has to be YYYY:MM:DD HH:MM:SS

    - Option 1:

    find . -name "*.jpg" | while read filename;
        exiftool "-AllDates=1986:11:05 12:00:00" "$filename";
    done

    - Option 2:

`exiftool "-AllDates=1986:11:05 12:00:00" -if '$filetype eq "JPEG"' .`

- Timeshift Photos by One Year

`exiftool "-AllDates+=1:0:0 0" .`

- Rename files to datestamp
    - Filename looks like 2014-01-01 12:00:00.jpg and will append -NUM if DateTimeOriginal is the same for multiple files

`exiftool '-FileName<DateTimeOriginal' -d "%Y-%m-%d %H.%M.%S%%-c.%%e" .`

- Rename Files to With Milliseconds
    - Good for burst photos where the seconds are all the same. If milliseconds are only out to 2 digits, use `${SubSecCreateDate}` instead

`exiftool -v '-Filename<${datetimeoriginal}${subsectimeoriginal;$_.=0 x(3-length)}.%e' -d %Y%m%d_%H%M%S .`

- Update any photo that doesn't have DateTimeOriginal to have it based on file modify date

`exiftool '-datetimeoriginal<filemodifydate' -if '(not $datetimeoriginal or ($datetimeoriginal eq "0000:00:00 00:00:00")) and ($filetype eq "JPEG")' .`

- Set DateTimeOriginal to Any Arbitrary Timestamp

`exiftool '-datetimeoriginal=2015:01:18 12:00:00' .`

## Moving/Copying Files ##

- Copy directory recursively into organized folder

`exiftool -o ~/dummy/ -if '$filesize# > 300000' '-Directory<CreateDate' -d ~/Desktop/old_photos2/%Y/%m\ %B -r ~/Desktop/iPhoto\ Library/`
