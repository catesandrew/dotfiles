# otherwise python raises errors about locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

python3 make-png.py
./png-to-icon.sh

# cleanup
rm -rf ./*.iconset ./*.png
