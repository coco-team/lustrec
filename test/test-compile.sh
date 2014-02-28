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
        $LUSTREC -d build -verbose 0 $opts -node $main ../$file;
        if [ $? -ne 0 ]; then
          rlustrec1="INVALID";
        else
          rlustrec1="VALID"
	fi
        gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ `basename $file .lus`.c > /dev/null
        if [ $? -ne 0 ]; then
          rgcc1="INVALID";
        else
          rgcc1="VALID"
	fi	
	# Removing Generated lusi file
	grep generated ../${file}i > /dev/null
	if [ $? -ne 1 ];then
	  rm ../${file}i
	fi
	# Checking inlining
	$LUSTREC -d build -verbose 0 $opts -inline -witnesses -node $main ../$file;
	if [ $? -ne 0 ]; then
          rlustrec2="INVALID";
        else
          rlustrec2="VALID"
	fi
        gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ `basename $file .lus`.c > /dev/null
        if [ $? -ne 0 ]; then
          rgcc2="INVALID";
        else
          rgcc2="VALID"
	fi	
	# Cheching witness
	lustreh -horn -node check witness.lus 2>/dev/null
    	z3="`z3 -t:10 witness.smt2 | xargs`"
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
	echo "lustrec ($rlustrec1),gcc($rgcc1),lustrec inline ($rlustrec2), gcc inline ($rgcc2), inlining valid ($rz3),`dirname $file`,`basename $file`,node $main" | column -t -s',' | tee -a ../report-$NOW | grep "INVALID\|ERROR\|UNKNOWN"
    else
	$LUSTREC -d build -verbose 0 $opts ../$file
	echo "lustrec ($rlustrec), gcc($rgcc), `dirname $file`,`basename $file`,node $main" | column -t -s',' | tee -a ../report-$NOW | grep "INVALID\|ERROR\|UNKNOWN"
    fi
done < ../tests_ok.list
