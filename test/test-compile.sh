#!/bin/bash

NOW=`date "+%y%m%d%H%M"`
LUSTREC="../../_build/src/lustrec"
mkdir -p build
cd build

while IFS=, read -r file main opts
do
#   echo fichier:$file
#   echo main:$main
#   echo opts:$opts
    if [ "$main" != "" ]; then
	$LUSTREC -horn -d build -verbose 0 $opts -node $main ../$file;
    else
	$LUSTREC -horn -d build -verbose 0 $opts ../$file 
    fi
    if [ $? -ne 0 ]; then 
      rlustrec="INVALID"; 
    else 
      rlustrec="VALID" 
    fi
    echo z3 `basename $file .lus`.smt2 | grep unsat 
    z3 `basename $file .lus`.smt2 | grep unsat > /dev/null
    if [ $? -ne 0 ]; then
      rz3="INVALID";
    else
      rz3="VALID"
    fi    
    echo "lustrec -horn ($rlustrec),z3 ($rz3),diff with ref ($rdiff),`dirname $file`,`basename $file`,node $main" | column -t -s',' | tee -a ../report-$NOW | grep INVALID
# awk 'BEGIN { FS = "\" " } ; { printf "%-20s %-40s\n", $1, $2, $3}' 
done < ../tests_ok.list
