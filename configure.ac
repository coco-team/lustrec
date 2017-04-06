define([gitversion], esyscmd([sh -c "git log --oneline | wc -l | tr -d '\n'"]))

AC_INIT([lustrec], [1.4-gitversion], [ploc@garoche.net])
AC_SUBST(VERSION_CODENAME, "Xia/Xiang-dev")
# Next release will be
#AC_INIT([lustrec], [1.5], [ploc@garoche.net])
#AC_SUBST(VERSION_CODENAME, "Xia/Shao Kang")

AC_CONFIG_SRCDIR([src/main_lustre_compiler.ml])
AC_CONFIG_SRCDIR([src/main_lustre_testgen.ml])

# default prefix is /usr/local
AC_PREFIX_DEFAULT(/usr/local)

AC_ARG_WITH([ocamlgraph-path],
        [AS_HELP_STRING([--ocamlgraph-path],
              [specify the path of ocamlgraph library. graph.cmxa should be in ocamlgraph-path @<:@default=$(ocamlfind query ocamlgraph)@:>@])],
        [AS_IF([test "x$ocamlgraph_path" = xno],
                [AC_MSG_ERROR([ocamlgraph library is needed])],
                [test "x$ocamlgraph_path" = xyes],
                [OCAMLGRAPH_PATH=$(ocamlfind query ocamlgraph)],
                [OCAMLGRAPH_PATH=$ocamlgraph_path]
        )],
        [OCAMLGRAPH_PATH=$(ocamlfind query ocamlgraph)]
)
AC_SUBST(OCAMLGRAPH_PATH)

AC_SUBST(SRC_PATH, esyscmd([sh -c "pwd" | tr -d '\n']))

AC_PATH_PROG([OCAMLC],[ocamlc],[:])
AC_MSG_CHECKING(OCaml version)
ocamlc_version=`$OCAMLC -v | grep version | rev| cut -d \  -f 1 | rev`
major=`echo $ocamlc_version | cut -d . -f 1`
minor=`echo $ocamlc_version | cut -d . -f 2`
if (test "$major" -lt 4 -a "$minor" -lt 0 ); then
  AC_MSG_ERROR([Ocaml version must be at least 4.0. You have version $ocamlc_version])
fi
AC_MSG_RESULT(valid ocaml version detected: $ocamlc_version)

AC_PATH_PROG([OCAMLBUILD],[ocamlbuild],[:])


# Checking libs

# Checks for libraries. OCamlgraph
AC_MSG_CHECKING(ocamlgraph library)
   ocamlgraph_lib=`find $OCAMLGRAPH_PATH -iname graph.cmxa | grep -m 1 -o "graph.cmxa"`
   if (test "x$ocamlgraph_lib" = xgraph.cmxa ); then
	ocamlgraph_lib_full=`find $OCAMLGRAPH_PATH -iname graph.cmxa | grep -m 1 "graph.cmxa"`
      AC_MSG_RESULT(library detected: $ocamlgraph_lib_full )
   else
      AC_MSG_ERROR([ocamlgraph library not installed in $OCAMLGRAPH_PATH])
   fi

AC_CHECK_LIB(gmp, __gmpz_init, 
      [gmp=yes],
      [AC_MSG_RESULT([GNU MP not found])
      gmp=no])

AC_CHECK_LIB(mpfr, mpfr_add, [mpfr=yes], 
		   [AC_MSG_RESULT(
[MPFR not found])
mpfr=no])


# Workaround to solve an issue with ocamlbuild and C libraries.
# oCFLAGS="$CFLAGS"
# CFLAGS="$FLAGS -Wl,--no-as-needed"
# AC_MSG_CHECKING([whether we need to add --no-as-needed linking option])
# AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],
#                                      [[
#                                         int main(){ return 0; }
#                                     ]])],
#                   [AC_MSG_RESULT([yes]); CC_NOASNEEDED="true"],
#                   [AC_MSG_RESULT([no]); CC_NOASNEEDED="false"])
# CFLAGS=$oCFLAGS
# AC_SUBST(CC_NOASNEEDED)


# End of config

AC_DEFUN([AC_DEFINE_DIR], [
  prefix_NONE=
  exec_prefix_NONE=
  test "x$prefix" = xNONE && prefix_NONE=yes && prefix=$ac_default_prefix
  test "x$exec_prefix" = xNONE && exec_prefix_NONE=yes && exec_prefix=$prefix
dnl In Autoconf 2.60, ${datadir} refers to ${datarootdir}, which in turn
dnl refers to ${prefix}.  Thus we have to use `eval' twice.
  eval ac_define_dir="\"[$]$2\""
  eval ac_define_dir="\"$ac_define_dir\""
  AC_SUBST($1, "$ac_define_dir")
  AC_DEFINE_UNQUOTED($1, "$ac_define_dir", [$3])
  test "$prefix_NONE" && prefix=NONE
  test "$exec_prefix_NONE" && exec_prefix=NONE
])

AC_DEFINE_DIR([abs_datadir], [datadir])

# Instanciation
AC_CONFIG_FILES([Makefile
		 src/Makefile
		 src/version.ml
		 test/test-compile.sh
		 ])

AC_OUTPUT


# summary
AC_MSG_NOTICE(******** Configuration ********)
AC_MSG_NOTICE(bin path:     $prefix/bin)
AC_MSG_NOTICE(include path: $prefix/include)
AC_MSG_NOTICE(********    Plugins    ********)

  if (test "x$gmp" = xyes -a "x$mpfr" = xyes ); then
       AC_MSG_NOTICE([-mpfr option enable])

   else 
       AC_MSG_WARN([MPFR option cannot be activated. Requires GMP and MPFR libs])
      
   fi
 
AC_MSG_NOTICE
AC_MSG_NOTICE(******** Configuration ********)

