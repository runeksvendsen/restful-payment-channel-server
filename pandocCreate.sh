#!/bin/bash

function prettyPrint {
   echo $1 | sed -e 's/^\.\///' -e 's/\.hs$//' -e 's/\//\./g'
}

set -e

SRCFILES=$(find $1 -iname "*.hs")

for FILE in $SRCFILES
do
   echo 'Processing "' $FILE '"'
   cat $FILE | \
      sed -e '1,/^import/d' | egrep -v "^import" | \
      awk '!NF {if (++n <= 2) print; next}; {n=0;print}' | \      
      sed -e 's/^/\^ /'
      #pandoc --variable mainfont="Palatino" --variable sansfont="Helvetica" --variable monofont="Menlo" \
      #   --variable fontsize=12pt --variable version=1.17.2 --latex-engine=xelatex \
      #   -o "$(prettyPrint $FILE).pdf"
exit 0
done

