# Project Setup
After cloning this repository, run the following commands:
```
opam switch create . ocaml-base-compiler.4.06.1
eval $(opam config env)
opam install merlin ocp-indent dune utop csv sexplib
```

Then, in order to recompile and execute the code, run the following:
```
dune clean && dune exec ./bin/main.exe tree [data]
```

Where data can be one of the following:
```
house-votes-84
votes-small
```
