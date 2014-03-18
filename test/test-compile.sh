#!/bin/bash

eval set -- $(getopt -n $0 -o "-aciwvh:" -- "$@")

declare c i w h a v
declare -a files

#SRC_PREFIX="../.."
SRC_PREFIX=`svn info --xml | grep wcroot | sed "s/<[^>]*>//g"`/lustre_compiler
NOW=`date "+%y%m%d%H%M"`
report=`pwd`/report-$NOW
#LUSTREC="../../_build/src/lustrec"
LUSTREC=lustrec
mkdir -p build
build=`pwd`"/build"
    

base_compile() {
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
	dir=${SRC_PREFIX}/`dirname "$file"`
	pushd $dir > /dev/null
    if [ "$main" != "" ]; then
	$LUSTREC -d $build -verbose 0 $opts -node $main "$name".lus;
        if [ $? -ne 0 ]; then
            rlustrec1="INVALID";
        else
            rlustrec1="VALID"
	fi
	pushd $build > /dev/null
        gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ "$name".c > /dev/null
	popd > /dev/null
        if [ $? -ne 0 ]; then
            rgcc1="INVALID";
        else
            rgcc1="VALID"
	fi	
    else
	$LUSTREC -d $build -verbose 0 $opts "$name".lus;
        if [ $? -ne 0 ]; then
            rlustrec1="INVALID";
        else
            rlustrec1="VALID"
        fi
	pushd $build > /dev/null
        gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ "$name".c > /dev/null
	popd > /dev/null
        if [ $? -ne 0 ]; then
            rgcc1="INVALID";
        else
            rgcc1="VALID"
        fi
    fi
    popd > /dev/null
    if [ $verbose -gt 0 ]; then
	echo "lustrec ($rlustrec1), gcc($rgcc1), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report;
    else
	echo "lustrec ($rlustrec1), gcc($rgcc1), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report | grep "INVALID\|ERROR\|UNKNOWN"
    fi;
    done < $file_list
}

inline_compile () {
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
	dir=${SRC_PREFIX}/`dirname "$file"`

	pushd $dir > /dev/null

# Checking inlining
    $LUSTREC -d $build -verbose 0 $opts -inline -witnesses -node $main "$name".lus;
    if [ $? -ne 0 ]; then
        rlustrec2="INVALID";
    else
        rlustrec2="VALID"
    fi
    pushd $build > /dev/null
    gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ "$name".c > /dev/null
    popd > /dev/null
    if [ $? -ne 0 ]; then
        rgcc2="INVALID";
    else
        rgcc2="VALID"
    fi	
    if [ $verbose -gt 0 ]; then
	echo "lustrec inlined ($rlustrec2), gcc ($rgcc2), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report;
    else
	echo "lustrec inlined ($rlustrec2), gcc ($rgcc2), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report | grep "INVALID\|ERROR\|UNKNOWN"
    fi;
    popd > /dev/null
done < $file_list
}

inline_compile_with_check () {
# Checking inlining
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
	dir=${SRC_PREFIX}/`dirname "$file"`
	pushd $dir > /dev/null
    $LUSTREC -d $build -verbose 0 $opts -inline -witnesses -node $main "$name".lus;
    if [ $? -ne 0 ]; then
        rlustrec2="INVALID";
    else
        rlustrec2="VALID"
    fi
    pushd $build > /dev/null
    gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ "$name".c > /dev/null
    popd > /dev/null
    if [ $? -ne 0 ]; then
        rgcc2="INVALID";
    else
        rgcc2="VALID"
    fi	
	# Cheching witness
    pushd $build > /dev/null
    $LUSTREC -horn -d $build/${name}_witnesses -node check $build/${name}_witnesses/inliner_witness.lus 2>/dev/null
    popd > /dev/null
    z3="`z3 -T:10 $build/${name}_witnesses/inliner_witness.smt2 | xargs`"
    if [ "x`echo $z3 | grep unsat`" == "xunsat" ]; then
	rinlining="VALID";
    elif [ "x`echo $z3 | xargs | grep -o error`" == "xerror" ]; then
	rinlining="ERROR";
    elif [ "x`echo $z3 | xargs | grep -o unknown`" == "xunknown" ]; then
	rinlining="UNKNOWN";
    else
	rinlining="INVALID"
	exit 1
    fi  
    if [ $verbose -gt 0 ]; then
	echo "lustrec inlined ($rlustrec2), gcc ($rgcc2), inlining valid ($rinlining), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report;
    else
	echo "lustrec inlined ($rlustrec2), gcc ($rgcc2), inlining valid ($rinlining), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report | grep "INVALID\|ERROR\|UNKNOWN"
    fi
    popd > /dev/null
done < $file_list

}

check_prop () {
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
	dir=${SRC_PREFIX}/`dirname "$file"`
	pushd $dir > /dev/null
	
    # Checking horn backend
    if [ "$main" != "" ]; then
	$LUSTREC -horn -d $build -verbose 0 $opts -node $main "$name".lus;
    else
	$LUSTREC -horn -d $build -verbose 0 $opts "$name".lus
    fi
    # echo "z3 $build/$name".smt2 
    # TODO: This part of the script has to be optimized
    z3 -T:10 "$build/$name".smt2 | grep unsat > /dev/null
    if [ $? -ne 0 ]; then
        rhorn="INVALID";
    else
        rhorn="VALID"
    fi
    if [ $verbose -gt 0 ]; then
	echo "horn-pdr ($rhorn), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report;
    else
	echo "horn-pdr ($rhorn), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report | grep "INVALID\|ERROR\|UNKNOWN"
    fi
    popd > /dev/null
done < $file_list
}

usage () {
echo "usage: $0 [-aciwh] file_list"
echo "-a: perform all steps"
echo "-c: basic compilation"
echo "-i: compile with inline mode"
echo "-w: compile with inline mode. Check the inlining with z3"
echo "-h: check files with the horn-pdf backend (requires z3)"
echo "-v <int>: verbose level"
}

verbose=0
nobehavior=1

while [ $# -gt 0 ] ; do
        case "$1" in
	        -v) shift ; verbose="$1"; shift ;;
	        -a) nobehavior=0; c=1 ; w=1; h=1; shift ;;
                -c) nobehavior=0; c=1 ; shift ;;
                -i) nobehavior=0; i=1 ; shift ;;
                -w) nobehavior=0; w=1 ; shift ;;
                -h) nobehavior=0; h=1 ; shift ;;
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

[ ! -z "$c" ] && base_compile
[ ! -z "$i" ] && inline_compile
[ ! -z "$w" ] && inline_compile_with_check
[ ! -z "$h" ] && check_prop
[ "$nobehavior" -eq 1 ] && echo "Must provide an argument in [aciwh]" && usage


	# Removing Generated lusi file
	#grep generated ../${file}i > /dev/null
	#if [ $? -ne 1 ];then
	#  rm ../${file}i
	#fi

