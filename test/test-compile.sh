#!/bin/bash

NOW=`date "+%y%m%d%H%M"`

mkdir -p build
cd build

while IFS=, read -r file main opts
do
#   echo fichier:$file
#   echo main:$main
#   echo opts:$opts
    if [ "$main" != "" ]; then
	lustrec -d build -verbose 0 $opts -node $main ../$file;
    else
	lustrec -d build -verbose 0 $opts ../$file
    fi
    if [ $? -ne 0 ]; then 
      rlustrec="INVALID"; 
    else 
      rlustrec="VALID" 
    fi
    gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ `basename $file .lus`.c > /dev/null
    if [ $? -ne 0 ]; then
      rgcc="INVALID";
    else
      rgcc="VALID"
    fi    
    echo "lustrec ($rlustrec),gcc ($rgcc),diff with ref ($rdiff),`dirname $file`,`basename $file`,node $main" | column -t -s',' | tee -a ../report-$NOW | grep INVALID
# awk 'BEGIN { FS = "\" " } ; { printf "%-20s %-40s\n", $1, $2, $3}' 
done < ../tests_ok.list
