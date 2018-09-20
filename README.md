
# FaCT

## Installation

To install you can either build the source or download ```fact.byte```. We recommend to build from source if possible.

```fact.byte``` is the executable used to compile FaCT programs. Execute ```./fact.byte -help``` for a list of the command line options.

## Usage

#### Basic Usage

Run ```./fact.byte ex.fact``` to compile a FaCT program where ```ex.fact``` is the name of your file.

#### Debugging

Many debugging options and intermediate data structures are available. Run ```./fact.byte -help``` for all options.

## Set Up And Build On Local Machine

FaCT is developed using Ocaml and LLVM 6.0. Make sure both of these are installed.
On OS X it can be done with brew.

```
brew install ocaml
brew install llvm60
```

We also need dependency management for Ocaml.

```brew install opam```

Then we need the actual dependencies for Ocaml.

```
opam switch 4.06.0
eval $(opam config env)
opam switch import ocamlswitch.txt
```

If you have not setup oasis, then you must do that first.

```oasis setup```

Make sure the Z3 lib is in the path:

```export LD_LIBRARY_PATH="$HOME/.opam/4.06.0/lib/z3"```

Finally we can build the compiler.

```make```

If you want to add a dependency, add it to ```_oasis```, then run the 3 previous commands again.

This will give us the ```fact.byte``` executable.

To install FaCT, run the command

```make install```

This will add FaCT to your path so that you can compile const files with the command,

```fact```

## Link to a C library

FaCT is designed to be called from C code. In order to do so, write your FaCT functions and compile them. This will output an object file. This can then be linked to a C file. For example:

```fact ex.fact```

Then we compile the calling C file:

```
clang -c main.c
```

Next, we link them together:

```clang -o final main.o ex.o```

Finally, we can run the executable:

```./final```
