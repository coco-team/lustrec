#!/bin/bash

NOW=`date "+%y%m%d%H%M"`
#LUSTREC="../../_build/src/lustrec"
LUSTREC=lustrec
mkdir -p build
cd build

while IFS=, read -r file main opts
do
  echo fichier:$file
#   echo main:$main
 #  echo opts:$opts
   rm -f witness*
    if [ "$main" != "" ]; then
	$LUSTREC -d build -verbose 0 $opts -inline -witnesses -node $main ../$file;
    else
	$LUSTREC -d build -verbose 0 $opts ../$file
    fi
    if [ $? -ne 0 ]; then 
      rlustrec="INVALID"; 
    else 
      rlustrec="VALID" 
    fi
  #  echo "horn encoding: lustreh -horn -node check witness.lus 2>/dev/nul"
    lustreh -horn -node check witness.lus 2>/dev/null
    #    gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ `basename $file .lus`.c > /dev/null
   # echo "Calling z3: "
    z3="`z3 -t:10 witness.smt2 | xargs`"
    #echo "Test: $z3"
    if [ "x`echo $z3 | grep unsat`" == "xunsat" ]; then
      rz3="VALID";
    elif [ "x`echo $z3 | xargs | grep -o error`" == "xerror" ]; then
      rz3="ERROR";
    elif [ "x`echo $z3 | xargs | grep -o unknown`" == "xunknown" ]; then
      rz3="UNKNOWN";
    else
      rz3="INVALID"
      exit 1
    fi    
    echo "lustrec ($rlustrec),inlining valid ($rz3),`dirname $file`,`basename $file`,node $main" | column -t -s',' | tee -a ../report-$NOW | grep "INVALID\|ERROR\|UNKNOWN"
# awk 'BEGIN { FS = "\" " } ; { printf "%-20s %-40s\n", $1, $2, $3}' 
done < ../tests_ok.nolarge.list
