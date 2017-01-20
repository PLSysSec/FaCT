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

 ```brew install ocaml```
 ```brew install llvm38```

We also need dependency management for Ocaml.

 ```brew install opam```

Then we need the actual dependencies for Ocaml.

 ```opam install llvm.3.8 core ounit ctypes-foreign utop dolog menhir oasis```

If you have not setup oasis, then you must do that first.

 ```oasis setup```

Then we must configure oasis.

 ```make configure```

Finally we can build the compiler.

 ```make build```.

If you want to add a dependency, add it to ```_oasis```, then run the 3 previous commands again.

This will give us the ```constanc.byte``` executable.

## Link to a C library

Constantc is designed to be called from C code. In order to do so, write your constanc functions and compile them. This will output an object file. This can then be linked to a C file. A full working example is in the `example` directory. First, we must compile ```main.c``` in the `example` directory:

 ```cd example/```
 
 ```clang -c main.c```

Then we compile ```ex.const``` using constanc. This requires clang to use version ≥3.8:

 ```../constanc.byte ex.const```

Next, we link them together:

 ```clang -o final main.o ex.o```

Finally, we can run the executable:

 ```./final```
