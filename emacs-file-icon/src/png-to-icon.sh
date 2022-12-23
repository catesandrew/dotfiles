for name in *.png ; do
    filename="${name%.*}"
    ./make-iconset.sh "$name"
done

for iconset in *.iconset ; do
    iconutil -c icns "$iconset"
done

mv ./*.icns ../icns/

echo 'png-to-icon done'
