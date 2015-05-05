#!/bin/sh
case "$OCAML_VERSION,$OPAM_VERSION" in
    4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
    4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
    4.02.1,1.1.0) ppa=avsm/ocaml42+opam11 ;;
    4.02.1,1.2.0) ppa=avsm/ocaml42+opam12 ;;
    *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "\n" | add-apt-repository ppa:$ppa
