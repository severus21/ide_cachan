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

```bash
make release
./ide.release
```
## Doc
To build doc, run

```bash
make doc
```

The documentation of the ide can be found in ```ide.docdir/index.html```
The documentation of the plugins can be found in ```plugins/a_plugin/doc/index.html```

## Adding a new subdir in D1/../Dn
```
mkdir D1/../Dn/subdir
echo "Dn/Subdir" >> D1/../Dn.mlpack
echo "<D1/../Dn/subdir/*.cmx>: for-pack(D1. .. .Dn.Subdir)
```

## Generating doc for a new module
1. Write doc in your module following the [ocamldoc rules](http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html)
2. Register full path of the module in ide.odocl
