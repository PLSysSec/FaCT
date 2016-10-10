### OCaml + LLVM

#### Instructions for OS X

1. install ocaml -> ```brew install ocaml```
2. install opam -> ```brew install opam```
3. install llvm -> ```brew install llvm```
4. Run ```opam init```
5. install ocaml dependencies with opam
    * ```opam install llvm``` (Make sure the version installed here is the same as what brew installed)
    * ```opam install core```
    * ```opam install ounit```
    * ```opam install ctypes-foreign```
    * ```opam install utop```. If unfamiliar with OCaml, read a utop tutorial to start. It is an Ocaml REPL
6. ```cd``` into this directory
7. compile the program -> ```ocamlbuild -use-ocamlfind -pkgs llvm,oUnit main.byte```
8. execute the program -> ```./_build/main.byte```

#### Setting up a custom utop

For some reason, utop doesn't like working with LLVM. Fortunately, this is an easy fix.
From the ```ml``` directory, execute ```ocamlfind ocamlmktop -o llvmutop -thread -linkpkg -package utop -package llvm -package llvm.passmgr_builder llvmutop.ml -cc g++```. This will build an executable, ```llvmutop```. Use this executable as the REPL.

#### TODO

1. Generalize the optimization accomplished with this goal.
2. Add types. Right now, the only type allowed are 32 bit ints. This is a hefty task. We need to add types to the AST and a type checker that maintains a type environment.
3. Generalize optimizations with a context. Optimizations should be designed as plugins. With this design, plugins may need to maintain a context(What have they seen? Is there any special information they need?)
4. Create more optimizations.
