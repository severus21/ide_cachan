# ide_cachan [![Build Status](https://travis-ci.org/severus21/ide_cachan.svg?branch=master)](https://travis-ci.org/severus21/ide_cachan)
Projet génie logiciel 2016 de l'ENS Cachan

## Building

To buid this project, ocaml 4.04.0 is required

```bash
opam update
opam switch 4.04.0
```

You will need the `lablgtk` and `ounit` modules and `ocamlbuild`
```bash
opam install lablgtk ounit ocamlbuild
```

## Adding a new subdir in D1/../Dn
```
mkdir D1/../Dn/subdir
echo "Dn/Subdir" >> D1/../Dn.mlpack
echo "<D1/../Dn/subdir/*.cmx>: for-pack(D1. .. .Dn.Subdir)
```
