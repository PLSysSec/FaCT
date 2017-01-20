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

1. ```opam install llvm.3.8 core ounit ctypes-foreign utop dolog menhir oasis```

If you have not setup oasis, then you must do that first.

1. ```oasis setup```

Then we must configure oasis.

1. ```make configure```

Finally we can build the compiler.

1. ```make build```.

If you want to add a dependency, add it to ```_oasis```, then run the 3 previous commands again.

This will give us the ```constanc.byte``` executable.

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
