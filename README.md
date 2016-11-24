## OCaml + LLVM

### Semantic tests
The semantic tests are written in C and call constantc functions. The purpose is to make sure constantc functions return the right value.

1. ```ocamlbuild -tag bin_annot -tag debug -I src -I test -use-ocamlfind -tag thread -pkgs llvm,oUnit,core test_semant_driver.byte```
2. ```./test_semant_driver.byte```
3. ```./final```

### Link to a C library
1. Compile constantc ```ocamlbuild -tag bin_annot -tag debug -I src -I test -use-ocamlfind -tag thread -pkgs llvm,oUnit,core constantc.byte```
2. Compile a constantc program ```./constantc.byte compile```. A default program is provided right now. This will change in the future.
3. Link constantc program ```./constantc.byte link```
4. Assemble a constantc program ```./constantc.byte assemble```
5. Create constantc shared object ```./constantc.byte share```
6. Compile C program that uses constantc function ```gcc -c harness.c```
7. Link C and constantc programs ```./constantc.byte harness```
8. Run C program! ```./final```

Alternatively, you can use the ```all``` command to compile, link, and run all the steps in between.

Finally, use the ```clean``` command to remove all the old temporary files. This is necessary in order to recompile the constantc compiler.

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

#### Setting up a custom utop

For some reason, utop doesn't like working with LLVM. Fortunately, this is an easy fix.
From the ```ml``` directory, execute ```ocamlfind ocamlmktop -o llvmutop -thread -linkpkg -package core -package utop -package llvm -package llvm.passmgr_builder llvmutop.ml -cc g++```. This will build an executable, ```llvmutop```. Use this executable as the REPL.

#### Writing and running tests

1. Create a file in the ```test``` directory. Write your unit tests.
2. Open your file in the ```test.ml``` file.
3. Build the tests. ```ocamlbuild -tag bin_annot -I src -I test -use-ocamlfind -pkgs llvm,oUnit test.byte```
4. Run the tests. ```./test.byte```
