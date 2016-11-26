#!/bin/bash

OPAM_DEPENDS="ocamlfind lablgtk ounit ocamlbuild"
LIB_DEPENDS="texlive-science libgtk2.0-dev libcanberra-gtk-module"
COMPILER_DEPENDS="make ocaml ocaml-native-compilers camlp4-extra opam"
TESTING_DEPENDS=""

#wget http://download.opensuse.org/repositories/home:ocaml/Debian_7.0/Release.key
#sudo apt-key add - < Release.key
#sudo echo 'deb http://download.opensuse.org/repositories/home:/ocaml/Debian_7.0/ /' >> /etc/apt/sources.list.d/opam.list

sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -qq ${COMPILER_DEPENDS}  ${LIB_DEPENDS} ${TESTING_DEPENDS}

export OPAMYES=1
opam init
opam switch 4.04.0
eval `opam config env`

echo "let () =
        try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
        with Not_found -> ()
  ;;">> ~/.ocamlinit

unset OCAMLLIB

opam install ${OPAM_DEPENDS}


