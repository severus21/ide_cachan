#!/bin/bash

OPAM_DEPENDS="ocamlfind ocambuild oUnit compiler-libs.common lablgtk2"
LIB_DEPENDS="texlive-science tex dvips ps2pdf libgtk2.0-dev"
COMPILER_DEPENDS="make"
TESTING_DEPENDS=""

wget http://download.opensuse.org/repositories/home:ocaml/Debian_7.0/Release.key
apt-key add - < Release.key
echo 'deb http://download.opensuse.org/repositories/home:/ocaml/Debian_7.0/ /' >> /etc/apt/sources.list.d/opam.list

sudo apt-get update -qq
sudo apt-get install -qq opam ocaml ${LIB_DEPENDS} ${COMPILER_DEPENDS} ${TESTING_DEPENDS}

opam init
opam switch 4.04.0

eval `opam config env`
opam install ${OPAM_DEPENDS}


