# https://gist.github.com/adriansr/1da9b18a8076b0f8a977a5eea0ae41ef
image=$1
dir="${image%.*}.iconset"
mkdir "$dir"
cp $image "${dir}/icon_512x512@2x.png"
convert $image -resize 512x512 "${dir}/icon_512x512.png"
convert $image -resize 512x512 "${dir}/icon_256x256@2x.png"
convert $image -resize 256x256 "${dir}/icon_256x256.png"
convert $image -resize 256x256 "${dir}/icon_128x128@2x.png"
convert $image -resize 128x128 "${dir}/icon_128x128.png"
convert $image -resize 64x64 "${dir}/icon_32x32@2.png"
convert $image -resize 32x32 "${dir}/icon_32x32.png"
convert $image -resize 32x32 "${dir}/icon_16x16@2x.png"
convert $image -resize 16x16 "${dir}/icon_16x16.png"
