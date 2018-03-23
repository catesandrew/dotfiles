#!/bin/bash
D=/Users/andrew/Desktop
C=$D/count.txt
if [ ! -f $C ]; then
  echo "0" > $C
fi

COUNT=`cat $C`
COUNT=`expr $COUNT + 1`
echo $COUNT > $C
P="`printf "%04d" $COUNT`_${MM_BUNDLE_ITEM_NAME}"

# printenv >$D/${P}_env.txt

tee $D/${P}_msg.txt
