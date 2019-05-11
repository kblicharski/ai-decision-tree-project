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

4. Running K-fold cross validation to determine the best depth up to a specified maximum depth.
```
dune clean && dune exec ./bin/main.exe kfold [data] [maxDepth]
```

Where `data` can be one of the following:
```
house-votes-84
balance-scale
car
tic-tac-toe
```

And `maxDepth` is an integer greater than 0. Note that only one value is necessary
to pass into the `classify` program because trees are written to files that
depend on the values of `data`. Thus, explicit file names are not required to be
specified. The only requirement is that `dtl` be run prior to `classify` or else
the program will fail because no tree has been built.

## Interpreting a decision tree
Output format for decision trees is in S-expression format. The format for a node
looks something like the following:

```
  ((NODE
    (5 middle-left-square x 0.0000
      ((LEAF (6 negative x)) (LEAF (6 positive o))
      (LEAF (6 positive b)))))
```

In this example we have a `NODE` that is at a depth of 5 where `middle-left-square`
is the attribute selected to split on next. There are 3 leafs -- each at depth 6 where
the first leaf's `middle-left-square` had an `x` and the classifier classified it as `negative`
