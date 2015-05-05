#!/bin/sh
case "$OCAML_VERSION,$OPAM_VERSION" in
    3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
    3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
    3.12.1,1.2.0) ppa=avsm/ocaml312+opam12 ;;
    4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
    4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
    4.00.1,1.2.0) ppa=avsm/ocaml40+opam12 ;;
    4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
    4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
    4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
    4.02.1,1.1.0) ppa=avsm/ocaml42+opam11 ;;
    4.02.1,1.2.0) ppa=avsm/ocaml42+opam12 ;;
    *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "\n" | add-apt-repository ppa:$ppa
