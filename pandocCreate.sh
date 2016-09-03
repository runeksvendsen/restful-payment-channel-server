#!/bin/bash

## Replace / with .
function prettyPrint {
   echo $1 | sed -e 's/^\.\///' -e 's/\.hs$//' -e 's/\//\./g'
}

set -e

if [ -z $1 ]; then
   echo "Usage: $0 <hs_source_code_dir>"
fi 

WORKDIR=$(pwd)

for DIR in $@
do   
   cd "$WORKDIR"
   cd "$DIR"
   SRCFILES=$(find . -iname "*.hs")

   for FILE in $SRCFILES
   do
      PNAME="$(prettyPrint $FILE)"
      FILENAME="$PNAME.html"

      echo "Processing $FILE..."
   
      cat $FILE | \
         sed -e '1,/^import/d' | egrep -v "^import" | \
         awk '!NF {if (++n <= 2) print; next}; {n=0;print}' | \
         HsColour -html | sed 's/Cyan/Brown/g' > "$FILENAME"

      sed -i "" -e "s/<body>/<body><center><h1>$PNAME<\/h1><\/center>/" "$FILENAME"

      cat "$FILENAME" | pandoc -t html5 -o "$PNAME.pdf"
      rm "$FILENAME"
      echo "Wrote $PNAME.pdf"
   done
done
