
Current Status:    [![Build Status](https://travis-ci.org/coco-team/lustrec.svg?branch=master)](https://travis-ci.org/coco-team/lustrec)

[![Stories in Ready](https://badge.waffle.io/coco-team/lustrec.png?label=ready&title=Ready)](https://waffle.io/coco-team/lustrec)
[![Throughput Graph](https://graphs.waffle.io/coco-team/lustrec/throughput.svg)](https://waffle.io/coco-team/lustrec/metrics/throughput)

# LustreC

LustreC is a modular compiler of Lustre code into C and Horn Clauses.

# Dependencies
On a fresh ubuntu/debian-like install
> apt-get install opam libmpfr-dev
Get a fresh version of ocaml
> opam switch 4.06.1
Install some dependencies
> opam install depext ocamlgraph mlmpfr num cmdliner fmt logs yojson menhir
# Build
```
> autoconf
> ./configure
> make
```

# Usage
```
> ./bin/lustrec -help
```

# People
* Pierre-Loic Garoche (ONERA)
* Xavier Thirioux (IRIT)
* Temesghen Kahsai (NASA Ames / CMU)
