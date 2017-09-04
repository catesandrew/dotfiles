#!/bin/bash

# convert all spaces to hyphens, remove characters like {}(),\! and convert the filename to lowercase.
find . -mindepth 1 -not \( -wholename "./.git" -prune \) | while ifs= read -r file; do
  mv "$file" "$(echo "$file" | tr ' ' '-' | tr -d '[{}(),\!]' | tr -d "\'" | tr '[A-Z]' '[a-z]' | sed 's/_-_/_/g' | sed 's/&/and/g')"
done

# just spaces to dashes
find . -mindepth 1 -not \( -wholename "./.git" -prune \) | while ifs= read -r FILE; do
  mv "$FILE" "$(echo "$FILE" | tr ' ' '-')"
done
