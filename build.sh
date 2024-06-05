#!/bin/sh -eux

autoconf
./configure --enable-local --disable-ide --disable-web-ide --disable-coq-libs --disable-pvs-libs --disable-isabelle-libs --disable-doc --disable-pdf-doc --disable-emacs-compilation
make clean
make
