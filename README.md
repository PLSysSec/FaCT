# Constantc

## Installation

To install you can either build the source or download ```constanc.byte```. We recommend to build from source if possible.

```constanc.byte``` is the executable used to compile Constantc programs. Execute ```./constanc.byte``` for a list of the command line options.

## Usage

#### Basic Usage

Run ```./constanc.byte ex.const``` to compile a Constantc program where ```ex.const``` is the name of your file.

#### Debugging

Many debugging options and intermediate data structures are available. Run ```./constanc.byte -help``` for all options.

## Set Up And Build On Local Machine

Constantc is developed using Ocaml and LLVM 3.8. Make sure both of these are installed.
On OS X it can be done with brew.

1. ```brew install ocaml```
2. ```brew install llvm38```

We also need dependency management for Ocaml.

1. ```brew install opam```

Then we need the actual dependencies for Ocaml.

1. ```opam install llvm.3.8 core ounit ctypes-foreign utop dolog menhir```

Finally we can build the compiler.

1. ```ocamlbuild -tag bin_annot -tag debug -I src -use-ocamlfind -use-menhir -tag thread -pkgs llvm,oUnit,core,dolog -no-hygiene constanc.byte```.

This will give us the ```constanc.byte``` executable. For an even faster executable(but not cross-platform), change ```byte``` to ```native```.

## Link to a C library

Constantc is designed to be called from C code. In order to do so, write your constanc functions and compile them. This will output an object file. This can then be linked to a C file. A full working example is below.

```c
#include <stdlib.h>

/**
    Filename: main.c
*/

// This function is defined in ex.const
int get100();

int main(void) {
  printf("%d\n", get100());
  return 1;
}

```

```c
/**
    Filename: ex.const
*/

int get100() {
  return 100;
}

```

First, we must compile ```main.c```. Using gcc, the command is

1. ```gcc -c main.c```

Then we compile ```ex.const``` using constanc. The command is

1. ```./constanc ex.const```

Next, we link them together. Using gcc, the command is

1. ```gcc -o final main.o ex.o```

Finall, we can run the executable with

1. ```./final```

## Semantic tests
The semantic tests are written in C and call constanc functions. The purpose is to make sure constanc functions return the right value.

1. ```ocamlbuild -tag bin_annot -tag debug -I src -I test -use-ocamlfind -tag thread -pkgs llvm,oUnit,core,dolog test_semant_driver.byte```
2. ```./test_semant_driver.byte```
3. ```./final```

## Setting up a custom utop

For some reason, utop doesn't like working with LLVM. Fortunately, this is an easy fix.
From the ```ml``` directory, execute ```ocamlfind ocamlmktop -o llvmutop -thread -linkpkg -package core -package utop -package llvm -package llvm.passmgr_builder llvmutop.ml -cc g++```. This will build an executable, ```llvmutop```. Use this executable as the REPL.

## Writing and running tests

1. Create a file in the ```test``` directory. Write your unit tests.
2. Open your file in the ```test.ml``` file.
3. Build the tests. ```ocamlbuild -tag bin_annot -I src -I test -use-ocamlfind -pkgs llvm,oUnit,core,dolog test.byte```
4. Run the tests. ```./test.byte```
