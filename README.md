# ide_cachan
Projet génie logiciel 2016 de l'ENS Cachan

## Building

You will need the `lablgtk` and `ounit` modules
```
opam install lablgtk ounit
```

## Adding a new subdir in D1/../Dn
```
mkdir D1/../Dn/subdir
echo "Dn/Subdir" >> D1/../Dn.mlpack
echo "<D1/../Dn/subdir/*.cmx>: for-pack(D1. .. .Dn.Subdir)
```
