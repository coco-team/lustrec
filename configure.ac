define([gitversion], esyscmd([sh -c "git log --oneline | wc -l | tr -d '\n'"]))
define([gitbranch], esyscmd([sh -c "git branch | grep \* | cut -d ' ' -f2"]))
AC_INIT([lustrec], 1.5-gitversion, [ploc@garoche.net])
AC_SUBST(VERSION_CODENAME, "Xia/Shao Kang-dev")
AC_SUBST(GITBRANCH, gitbranch)
# Next release will be
#AC_INIT([lustrec], [1.6], [ploc@garoche.net])
#AC_SUBST(VERSION_CODENAME, "Xia/Zhu")
AC_MSG_NOTICE(Git branch: ${GITBRANCH})
AC_CONFIG_SRCDIR([src/main_lustre_compiler.ml])
AC_CONFIG_SRCDIR([src/main_lustre_testgen.ml])

AC_PATH_PROG([OCAMLC],[ocamlc],[:])
AC_MSG_CHECKING(OCaml version)
ocamlc_version=`$OCAMLC -v | grep version | rev| cut -d \  -f 1 | rev`
major=`echo $ocamlc_version | cut -d . -f 1`
minor=`echo $ocamlc_version | cut -d . -f 2`
if (test "$major" -lt 3 -a "$minor" -lt 11 ); then
  AC_MSG_ERROR([Ocaml version must be at least 3.11. You have version $ocamlc_version])
fi
AC_MSG_RESULT(valid ocaml version detected: $ocamlc_version)

AC_PATH_PROG([OCAMLBUILD],[ocamlbuild],[:])


# default prefix is /usr/local
AC_PREFIX_DEFAULT(/usr/local)

dnl AC_ARG_WITH([ocamlgraph-path],
dnl         [AS_HELP_STRING([--ocamlgraph-path],
dnl               [specify the path of ocamlgraph library. graph.cmxa should be in ocamlgraph-path @<:@default=$(ocamlfind query ocamlgraph)@:>@])],
dnl         [AS_IF([test "x$ocamlgraph_path" = xno],
dnl                 [AC_MSG_ERROR([ocamlgraph library is needed])],
dnl                 [test "x$ocamlgraph_path" = xyes],
dnl                 [OCAMLGRAPH_PATH=$(ocamlfind query ocamlgraph)],
dnl                 [OCAMLGRAPH_PATH=$ocamlgraph_path]
dnl         )],
dnl         [OCAMLGRAPH_PATH=$(ocamlfind query ocamlgraph)]
dnl )
dnl AC_SUBST(OCAMLGRAPH_PATH)


# Checking libs
AC_CHECK_PROG(FINDLIB_CHECK,ocamlfind,yes)
if test x"$FINDLIB_CHECK" != x"yes" ; then
   AC_MSG_ERROR(ocamlfind required!)
fi
dnl AC_MSG_RESULT(Hourrah: ocamlfind found!)

# Checks for libraries.
# OCamlgraph
AC_MSG_CHECKING(ocamlgraph library)
AS_IF([ocamlfind query ocamlgraph >/dev/null 2>&1],
    [],[AC_MSG_ERROR(ocamlgraph required. opam install ocamlgraph should solve the issue)],
)
AC_MSG_RESULT(yes)

#Yojson
AC_MSG_CHECKING(yojson library)
AS_IF([ocamlfind query yojson >/dev/null 2>&1],
    [],[AC_MSG_ERROR(yojson required. opam install yojson should solve the issue)],
)
AC_MSG_RESULT(yes)

# logging and command line librairies
AC_MSG_CHECKING(cmdliner library)
AS_IF([ocamlfind query cmdliner >/dev/null 2>&1],
    [],[AC_MSG_ERROR(cmdliner required. opam install cmdliner should solve the issue)],
)
AC_MSG_RESULT(yes)

AC_MSG_CHECKING(fmt and logs library)
AS_IF([ocamlfind query fmt logs >/dev/null 2>&1],
    [],[AC_MSG_ERROR(fmt and logs required. opam install fmt logs should solve the issue)],
)
AC_MSG_RESULT(yes)

# Salsa
AC_ARG_ENABLE(salsa, [AS_HELP_STRING([--disable-salsa],
              [disable Salsa plugin. Enabled by default if available.])])


AC_MSG_CHECKING(salsa library)
AS_IF([ocamlfind query salsa >/dev/null 2>&1],
    [salsa=yes; AC_MSG_RESULT(yes)],[salsa=no; AC_MSG_WARN(no)]
)


AS_IF([test "x$enable_salsa" != "xno"], [
  if (test "x$salsa" = xyes ); then
   AC_SUBST(SALSA, "(module Salsa_plugin.Plugin : PluginType.PluginType);")
   AC_SUBST(SALSA_TAG, "<**/*.native> or <plugins/salsa/*.cm?> : package(salsa)")
  fi
])

# GMP
AC_CHECK_LIB(gmp, __gmpz_init,
      [gmp=yes],
      [AC_MSG_RESULT([GNU MP not found])
      gmp=no])

AC_CHECK_LIB(mpfr, mpfr_add, [mpfr=yes],
		   [AC_MSG_RESULT(
[MPFR not found])
mpfr=no])


# Checking availability of path to regression tests
tests_path="../lustrec-tests/regression_tests"

AC_ARG_WITH([tests-path],
            [AS_HELP_STRING([--with-tests-path],
               [provides path to test suite (default is ../lustrec-test if available)])],
            [tests_path="$withval";
	     if (test "x$tests_path" = xyes); then
	       AC_MSG_ERROR(Option --with-tests-path requires a parameter: eg. --with-tests-path=value);
	     fi],
            )
AC_MSG_NOTICE($tests_path)
AC_CHECK_FILE(${tests_path}/CMakeLists.txt,
              [
	      valid_test_path=true
	      ],
	      [
	      valid_test_path=false
	        AC_SUBST(PATH_TO_TESTS, $tests_path)	      ])
AC_SUBST(PATH_TO_TESTS_DEFINED, $valid_test_path)
AC_SUBST(PATH_TO_TESTS, $tests_path)

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
		 src/pluginList.ml
		 src/_tags
		 ])

AC_OUTPUT


# summary
AC_MSG_NOTICE(******** Configuration ********)
AC_MSG_NOTICE(bin path:     $prefix/bin)
AC_MSG_NOTICE(include path: $prefix/include)
AC_MSG_NOTICE(********    Plugins    ********)

  if (test "x$gmp" = xyes -a "x$mpfr" = xyes ); then
      AC_MSG_NOTICE([MPFR option enabled])
  else
      AC_MSG_WARN([MPFR option cannot be activated. Requires GMP and MPFR libs])
  fi

  if (test "x$salsa" = xyes -a "x$enable_salsa" != xno); then
      AC_MSG_NOTICE([Salsa plugin enabled])
  else
      AC_MSG_NOTICE([Salsa plugin disabled])
  fi
AC_MSG_NOTICE(****** Regression Tests  ******)
if (test "x$valid_test_path" = xfalse); then
  AC_MSG_NOTICE(no valid tests path provided ($tests_path))
else
AC_MSG_NOTICE(tests path: $tests_path)
fi
AC_MSG_NOTICE(******** Configuration ********)
AC_MSG_NOTICE(Execute "make; make install" now)
