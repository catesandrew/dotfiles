#!/bin/bash

#fortune=`fortune | cowsay`
#echo $fortune | sed "s/^/\[`date +"%Y%m%d%H%M%S"`]/"
#echo $fortune | while read line; do echo "[] $line"; done

#while read line
#do
    #echo "> $line"
#done | $fortune

#echo "--
#Andrew Cates

#`fortune | cowsay | sed -e 's/^/>  /'`" | emate mailto

#echo "--
#Andrew Cates

#`fortune | cowsay | sed -e 's/^/    /'`" | emate mailto

echo "

`fortune | sed -e 's/^/> /'`" | emate mailto
