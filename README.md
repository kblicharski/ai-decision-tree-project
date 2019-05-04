# Project Setup
After cloning this repository, run the following commands:
```
opam switch create . ocaml-base-compiler.4.06.1
eval $(opam config env)
opam install merlin ocp-indent dune utop csv
```

Then, in order to recompile and execute the code, run the following:
```
dune exec ./bin/main.exe
dune clean
```