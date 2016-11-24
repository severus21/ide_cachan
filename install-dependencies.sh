#!/bin/bash

OPAM_DEPENDS="ocamlfind ocambuild oUnit compiler-libs.common lablgtk2"
LIB_DEPENDS="texlive-science libgtk2.0-dev"
COMPILER_DEPENDS="make"
TESTING_DEPENDS=""

#wget http://download.opensuse.org/repositories/home:ocaml/Debian_7.0/Release.key
#sudo apt-key add - < Release.key
#sudo echo 'deb http://download.opensuse.org/repositories/home:/ocaml/Debian_7.0/ /' >> /etc/apt/sources.list.d/opam.list

sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -y opam
#eval $(opam config env)
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra ${LIB_DEPENDS} ${COMPILER_DEPENDS} ${TESTING_DEPENDS}

opam init --yes
opam switch 4.04.0 --yes
echo "/home/travis/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> ~/.profile
eval $(`opam config env`)
opam install ${OPAM_DEPENDS} --yes


