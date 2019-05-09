# Project Setup
After cloning this repository, run the following commands:
```
opam switch create . ocaml-base-compiler.4.06.1
eval $(opam config env)
opam install merlin ocp-indent dune utop csv sexplib
```

## Usages
1. Creating a decision tree
```
dune clean && dune exec ./bin/main.exe dtl [data]
```

2. Creating a decision tree, limited by a maximum depth.
```
dune clean && dune exec ./bin/main.exe dtl [data] [maxDepth]
```

3. Running the classifier on a generated decision tree
```
dune clean && dune exec ./bin/main.exe classify [data]
```

Where `data` can be one of the following:
```
house-votes-84
votes-small
```

And `maxDepth` is an integer greater than 0. Note that only one value is necessary
to pass into the `classify` program because trees are written to files that 
depend on the values of `data`. Thus, explicit file names are not required to be
specified. The only requirement is that `dtl` be run prior to `classify` or else
the program will fail because no tree has been built.