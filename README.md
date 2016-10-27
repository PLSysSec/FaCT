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
7. compile the program -> ```ocamlbuild -tag bin_annot -I src -I test -use-ocamlfind -pkgs llvm,oUnit main.byte```
8. execute the program -> ```.main.byte```

#### Run Main
In addition to tests, there is a main script. This contains a large AST to compile, and integrates all of the pieces of the compiler.

1. ```ocamlbuild -tag bin_annot -I src -I test -use-ocamlfind -pkgs llvm,oUnit main.byte```
2. ```./main.byte```
3. Execute the produced LLVM IR. ```lli out.ll```

#### Setting up a custom utop

For some reason, utop doesn't like working with LLVM. Fortunately, this is an easy fix.
From the ```ml``` directory, execute ```ocamlfind ocamlmktop -o llvmutop -thread -linkpkg -package utop -package llvm -package llvm.passmgr_builder llvmutop.ml -cc g++```. This will build an executable, ```llvmutop```. Use this executable as the REPL.

#### Writing and running tests

1. Create a file in the ```test``` directory. Write your unit tests.
2. Open your file in the ```test.ml``` file.
3. Build the tests. ```ocamlbuild -tag bin_annot -I src -I test -use-ocamlfind -pkgs llvm,oUnit test.byte```
4. Run the tests. ```./test.byte```

#### LLVM Documentation
https://llvm.moe/ocaml/

This page contains all of the LLVM Ocaml binding documentation. The most important module is ```Llvm```(https://llvm.moe/ocaml/Llvm.html).

Initially, this page can be daunting to navigate, but I have noticed some tricks to finding what you need.

1. All of the functions map to modules and functions in the C++ code. If you look at C++ documentation and want to call one of those functions, you can find it by searching for it here.

2. It can be tough to find the exact function/type needed. For instance if you need the boolean type, you can try searching for ```boolean```(in the ocaml docs). But you wont find anything(because booleans dont exist in LLVM IR). Instead, search for a type that you know, say ```i32_type```. Voila, this is grouped with all of the types in LLVM.

#### TODO

1. Generalize the optimization accomplished with this goal.
2. Add types. Right now, the only type allowed are 32 bit ints. This is a hefty task. We need to add types to the AST and a type checker that maintains a type environment. --> Started. We have a type checker. Also, the codegen supports ints and booleans. Next up is strings.
3. Generalize optimizations with a context. Optimizations should be designed as plugins. With this design, plugins may need to maintain a context(What have they seen? Is there any special information they need?)
4. Create more optimizations.
