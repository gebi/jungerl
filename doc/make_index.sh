#!/bin/sh

ROOTDIR="."

cat $ROOTDIR/doc/HEAD

echo "<dl>"
for d in `find $ROOTDIR/lib -type d -mindepth 1 -maxdepth 1 | sort`
do
   if [ "./lib/CVS" = $d ]; then continue; fi

   echo "<dt>" `basename $d` "</dt>"
   if [ -e $d/doc/short-desc ]
   then 
      echo "<dd> " | cat - $d/doc/short-desc
   else
      echo "<dd> No description in the file <code>$d/doc/short-desc</code> yet!"
   fi
   echo "</dd>"
done
echo "</dl>"

echo "<p>List updated " `LANG=C date` "</p>"

cat $ROOTDIR/doc/FOOT
