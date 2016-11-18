#!/bin/bash

# USAGE: mkthumb.sh [-s] <abs_path> [abs_path]
# create nautilus thumbnails for images and PDFs in the directories (and their
# sub-directories) given as parameters.
# -s is used to skip generating thumbnails that already exist

skip_existing=0
if [[ "${1}" == "-s" ]]; then
  skip_existing=1
  shift
fi

mkthumb() {
  file="${1}"
  dest="${2}"
  # convert -resize 512x512 120507\ -\ Minute\ Order.pdf -background none -gravity center -extent 512x512 test.png
  # -define png:compression-strategy=1
  convert -thumbnail 512x512 "${file}[0]" -background none -gravity center -extent 512x512 -define png:compression-level=1 -define png:compression-filter=5 -define png:compression-strategy=1 "${dest}" &>/dev/null
  # convert -thumbnail x512 -background white -alpha remove "${file}[0]" "${dest}" &>/dev/null
  # convert -thumbnail 512x512 "${file}[0]" "${dest}" &>/dev/null

  # pngquant --ext .png --force 32
  if (( $? == 0 )); then
    echo "OK   ${file}"
  else
    echo "FAIL ${file}"
  fi
}

# (pdf|png|jpg|gif|jpeg)
OLDIFS="${IFS}"
IFS=$'\n'
for dir in $@; do
  echo "Processing directory ${dir}"
  for file in $(gfind "${dir}" -regextype posix-egrep -iregex \
  '.*\.(pdf)'); do
    base=$(basename "${file}")
    md5=$(echo -n "${base}" | perl -MURI::file -MDigest::MD5=md5_hex -ne \
          'print md5_hex(URI::file->new($_));')

    dest="$(dirname "${file}")/.thumbnails"
    mkdir -p "${dest}"
    dest="${dest}/${md5}.png"
    # dest="${HOME}/.thumbnails/normal/${md5}.png"
    if [[ -f "${dest}" ]]; then
      if [[ "${skip_existing}" == "0" ]]; then
        mkthumb "${file}" "${dest}"
      else
        echo "SKIP ${file}"
      fi
    else
      mkthumb "${file}" "${dest}"
    fi
  done
done
IFS="${OLDIFS}"
