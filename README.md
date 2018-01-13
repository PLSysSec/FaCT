# FaCT

## Installation

To install you can either build the source or download ```fact.byte```. We recommend to build from source if possible.

```fact.byte``` is the executable used to compile FaCT programs. Execute ```./fact.byte``` for a list of the command line options.

## Usage

#### Basic Usage

Run ```./fact.byte ex.fact``` to compile a FaCT program where ```ex.fact``` is the name of your file.

#### Debugging

Many debugging options and intermediate data structures are available. Run ```./fact.byte -help``` for all options.

## Set Up And Build On Local Machine

FaCT is developed using Ocaml and LLVM 3.8. Make sure both of these are installed.
On OS X it can be done with brew.

```
brew install ocaml
brew install llvm38
```

We also need dependency management for Ocaml.

```brew install opam```

Then we need the actual dependencies for Ocaml.

```opam install llvm.3.8 core ounit ctypes-foreign utop dolog menhir oasis ppx_deriving```

The easiest way to install Z3 is the following:

```
opam remote add termite https://github.com/termite-analyser/opam-termite.git
opam install z3
export LD_LIBRARY_PATH="$(ocamlfind printconf destdir)/stublibs:${LD_LIBRARY_PATH}"
```

You may also need to symlink the ```libz3.so``` binary from ```$HOME/.opam/system/lib``` into
the ```stublibs``` directory, e.g. via:

```
ln -s $(ocamlfind printconf destdir)/libz3.so $(ocamlfind printconf destdir)/stublibs/
```

If you have not setup oasis, then you must do that first.

```oasis setup```

Then we must configure oasis.

```make configure```

If you're having trouble, try reconfiguring your opam env.

```eval $(opam config env)```

Finally we can build the compiler.

```make build```

If you want to add a dependency, add it to ```_oasis```, then run the 3 previous commands again.

This will give us the ```fact.byte``` executable.

To install FaCT, run the command

```make install```

This will add FaCT to your path so that you can compile const files with the command,

```fact```

## Link to a C library

FaCT is designed to be called from C code. In order to do so, write your FaCT functions and compile them. This will output an object file. This can then be linked to a C file. A full working example is in the `example` directory. First, we must compile ```main.c``` in the `example` directory:

```
cd example/
clang -c main.c
```

Then we compile ```ex.fact``` using FaCT. This requires clang to use version ≥3.8:

```../fact.byte ex.fact```

Next, we link them together:

```clang -o final main.o ex.o```

Finally, we can run the executable:

```./final ```
