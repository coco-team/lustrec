#!/bin/bash

eval set -- $(getopt -n $0 -o "-aciwvh:" -- "$@")

declare c i w h a v
declare -a files

SRC_PREFIX=/home/thirioux/RECHERCHE/lustrec-tests/
#SRC_PREFIX=`svn info --xml | grep wcroot | sed "s/<[^>]*>//g"`/lustre_compiler
NOW=`date "+%y%m%d%H%M"`
report=`pwd`/report-1.1-440-$NOW
LUSTREC=lustrec
mkdir -p build
build=`pwd`"/build"

gcc_compile() {
    if [ $verbose -gt 1 ]; then
	echo "gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ $1.c > /dev/null"
    fi
    gcc -c -Wall -Wno-unused-but-set-variable -I ../../include/ "$1".c > /dev/null;
    if [ $? -ne 0 ]; then
	rgcc="INVALID";
    else
	rgcc="VALID"
    fi
}

lustrec_compile() {
    if [ $verbose -gt 1 ]; then
       echo "$LUSTREC $@"
    fi
    $LUSTREC "$@";
    if [ $? -ne 0 ]; then
        rlustrec="INVALID";
    else
        rlustrec="VALID"
    fi
}

base_compile() {
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
        ext=".lus"
	if [ `dirname "$file"`/"$name" = "$file" ]; then
	    name=`basename "$file" .lusi`
	    ext=".lusi"
	fi
        dir=${SRC_PREFIX}/`dirname "$file"`
	pushd $dir > /dev/null

	if [ "$main" != "" ]; then
	    lustrec_compile -d $build -verbose 0 $opts -node $main $name$ext;
	else
	    lustrec_compile -d $build -verbose 0 $opts $name$ext
	fi
	pushd $build > /dev/null

	if [ $ext = ".lus" ] && [ "$opts" != "-lusi" ]; then
            gcc_compile "$name";
	else
	    rgcc="NONE"
	fi
	popd > /dev/null
	popd > /dev/null

	if [ $verbose -gt 0 ]; then
	    echo "lustrec ($rlustrec), gcc($rgcc), $dir, ${name}${ext}, node $main" | column -t -s',' | tee -a $report;
	else
	    echo "lustrec ($rlustrec), gcc($rgcc), $dir, ${name}${ext}, node $main" | column -t -s',' | tee -a $report | grep "INVALID\|ERROR\|UNKNOWN"
	fi;
    done < $file_list
}

inline_compile () {
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
	ext=".lus"
	if [ `dirname "$file"`/"$name" = "$file" ]; then
	    name=`basename "$file" .lusi`
	    ext=".lusi"
	fi
	dir=${SRC_PREFIX}/`dirname "$file"`
	pushd $dir > /dev/null

	if [ "$main" != "" ]; then
	    lustrec_compile -d $build -verbose 0 $opts -inline -witnesses -node $main $name$ext;
	else
	    if [ "$ext" = ".lusi" ]; then
		lustrec_compile -d $build -verbose 0 $opts $name$ext;
	    else
		rlustrec="NONE"
		rgcc="NONE"
	    fi
	fi
	pushd $build > /dev/null

	if [ "$main" != "" ] && [ $ext = ".lus" ] && [ "$opts" != "-lusi" ]; then
	    gcc_compile "$name";
	else
	    rgcc="NONE"
	fi
	popd > /dev/null
	popd > /dev/null

	if [ $verbose -gt 0 ]; then
	    echo "lustrec inlined ($rlustrec), gcc ($rgcc), $dir, ${name}${ext}, node $main" | column -t -s',' | tee -a $report;
	else
	    echo "lustrec inlined ($rlustrec), gcc ($rgcc), $dir, ${name}${ext}, node $main" | column -t -s',' | tee -a $report | grep "INVALID\|ERROR\|UNKNOWN"
	fi;
    done < $file_list
}

inline_compile_with_check () {
# Checking inlining
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
	ext=".lus"
	if [ `dirname "$file"`/"$name" = "$file" ]; then
	    name=`basename "$file" .lusi`
	    ext=".lusi"
	fi
	dir=${SRC_PREFIX}/`dirname "$file"`
	pushd $dir > /dev/null

	if [ "$main" != "" ]; then
	    lustrec_compile -d $build -verbose 0 $opts -inline -witnesses -node $main $name$ext;
	else
	    if [ "$ext" = ".lusi" ]; then
		lustrec_compile -d $build -verbose 0 $opts $name$ext;
	    else
		rlustrec="NONE"
		rgcc="NONE"
	    fi
	fi
	popd > /dev/null
	pushd $build > /dev/null
	
	if [ "$main" != "" ] && [ $ext = ".lus" ] && [ "$opts" != "-lusi" ]; then
	    gcc_compile "$name";
	else
	    rgcc="NONE"
	fi
	# Cheching witness
       
	if [ "$main" != "" ] && [ $ext = ".lus" ] && [ "$opts" != "-lusi" ]; then
	    mv ${name}_witnesses/inliner_witness.lus ${name}_inliner_witness.lus
	    lustrec_compile -verbose 0 -horn-traces -node check ${name}_inliner_witness.lus
	    z3="`z3 -T:10 ${name}_inliner_witness.smt2 | xargs`"
	    if [ "x`echo $z3 | grep -o unsat`" == "xunsat" ]; then
		rinlining="VALID";
	    elif [ "x`echo $z3 | xargs | grep -o error`" == "xerror" ]; then
		rinlining="ERROR";
	    elif [ "x`echo $z3 | xargs | grep -o unknown`" == "xunknown" ]; then
		rinlining="UNKNOWN";
	    elif [ "x`echo $z3 | xargs | grep -o timeout`" == "xtimeout" ]; then
		rinlining="TIMEOUT"
	    else
		rinlining="INVALID"
	    fi
	else
	    rinlining="NONE"
	fi
	popd > /dev/null

	if [ $verbose -gt 0 ]; then
	    echo "lustrec inlined ($rlustrec), gcc ($rgcc), inlining check ($rinlining), $dir, ${name}${ext}, node $main" | column -t -s',' | tee -a $report;
	else
	    echo "lustrec inlined ($rlustrec), gcc ($rgcc), inlining check ($rinlining), $dir, ${name}${ext}, node $main" | column -t -s',' | tee -a $report | grep "TIMEOUT\|INVALID\|ERROR\|UNKNOWN"
	fi
done < $file_list

}

check_prop () {
    while IFS=, read -r file main opts
    do
	name=`basename "$file" .lus`
	if [ "$name" = "$file" ]; then
	    return 0
	fi
	dir=${SRC_PREFIX}/`dirname "$file"`
	pushd $dir > /dev/null
	
    # Checking horn backend
    if [ "$main" != "" ]; then
	lustrec_compile -horn-traces -horn-query -d $build -verbose 0 $opts -node $main $name".lus";
    else
	lustrec_compile -horn-traces -horn-query -d $build -verbose 0 $opts $name".lus"
    fi

    # echo "z3 $build/$name".smt2 
    # TODO: This part of the script has to be optimized
    z3="`z3 -T:10 ${build}/${name}.smt2 | xargs`"
    if [ "x`echo $z3 | grep -o unsat`" == "xunsat" ]; then
	rhorn="VALID";
    elif [ "x`echo $z3 | xargs | grep -o error`" == "xerror" ]; then
	rhorn="ERROR";
    elif [ "x`echo $z3 | xargs | grep -o unknown`" == "xunknown" ]; then
	rhorn="UNKNOWN";
    elif [ "x`echo $z3 | xargs | grep -o timeout`" == "xtimeout" ]; then
	rhorn="TIMEOUT"
    else
	rhorn="INVALID"
    fi
    if [ $verbose -gt 0 ]; then
	echo "lustrec ($rlustrec), horn-pdr ($rhorn), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report;
    else
	echo "lustrec ($rlustrec), horn-pdr ($rhorn), $dir, ${name}.lus, node $main" | column -t -s',' | tee -a $report | grep "INVALID\|ERROR\|UNKNOWN"
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

# cleaning directory $build

rm -f "$build"/* 2> /dev/null

# executing tests

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

