#!/bin/bash

#eval set -- $(getopt -n $0 "-aciwvh:" -- "$@")

declare c i w h a v r
declare -a files

#SRC_PREFIX="../.."
#SRC_PREFIX=`svn info --xml | grep wcroot | sed "s/<[^>]*>//g"`/lustre_compiler
NOW=`date "+%y-%m-%d-%H:%M"`
report=`pwd`/horn-report-$NOW
#LUSTREC="../../_build/src/lustrec"
LUSTREC=../bin/lustrec
mkdir -p build-$NOW
build=`pwd`/build-$NOW


check_horn () {
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
	if [ "$name" = "$file" ]; then
	    return 0
	fi
	dir=${SRC_PREFIX}`dirname "$file"`
	pushd $dir > /dev/null

    # Checking horn backend
    if [ "$main" != "" ]; then
        $LUSTREC -horn-traces -horn-query -d $build -verbose 0 $opts -node $main "$name".lus
    else
	$LUSTREC -horn-traces -horn-query -d $build -verbose 0 $opts "$name".lus
    fi
    if [ $? -ne 0 ]; then
        rlustrec="ERROR";
    else
        rlustrec="OK"
    fi
    if [ $verbose -gt 0 ]; then
	echo "lustrec ($rlustrec), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report;
    else
	echo "lustrec ($rlustrec), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report | grep "INVALID\|ERROR\|UNKNOWN"
    fi
    popd > /dev/null
done < $file_list
}

usage () {
echo "usage: $0 [-aciwh] file_list"
echo "-r: regression test for horn backend"
echo "-v <int>: verbose level"
}

verbose=0
nobehavior=1

while [ $# -gt 0 ] ; do
        case "$1" in
	        -v) shift ; verbose="$1"; shift ;;
                -r) nobehavior=0; r=1 ; shift ;;
                --) shift ;;
                -*) echo "bad option '$1'" ; exit 1 ;;
                *) files=("${files[@]}" "$1") ; shift ;;
         esac
done

file_list=${files[0]}


if [ ${#files} -eq 0 ] ; then
    echo input list required
    usage
    exit 1
fi

[ ! -z "$r" ] && check_horn
mv $report $build
[ "$nobehavior" -eq 1 ] && echo "Must provide an argument in [aciwh]" && usage
