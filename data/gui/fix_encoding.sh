#! /bin/bash

#find data/gui -name '*.txt' -exec file {} \; | grep  "Unicode" | awk -F':' '{ print $1}' > unicode.txt

echo "Fixing ISO-8859"
find . -name '*.txt' -exec file {} \; | grep  "ISO-8859" | awk -F':' '{ print $1}' > iso-8859.txt
while read l
do
    echo $l
    iconv -f iso-8859-1 -t utf8 "$l" >"$l.new" && mv -f "$l.new" "$l"
done<iso-8859.txt

echo "Fixing custom encoding"
find . -name '*.txt' -exec file {} \; | grep  ".*Non-ISO.*" | awk -F':' '{ print $1}' > non-iso.txt
while read l
do
    echo $l
    python3 fix_custom_encoding.py "$l"
    iconv -f Latin10 -t utf8 "$l" >"$l.new" && mv -f "$l.new" "$l"
done<non-iso.txt

rm non-iso.txt
rm iso-8859.txt
