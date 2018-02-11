# FaCT

## Usage

#### Basic Usage

Run ```./fact.byte ex.fact``` to compile a FaCT program where ```ex.fact``` is the name of your file.

#### Debugging

Many debugging options and intermediate data structures are available. Run ```./fact.byte -help``` for all options.

## Set Up And Build On Local Machine

FaCT is developed using OCaml (https://ocaml.org/) and LLVM 3.8 (http://releases.llvm.org/). Make sure both of these are installed.
On OS X it can be done with brew.

```
brew install ocaml
brew install llvm38
```

We also need dependency management for Ocaml (https://opam.ocaml.org/).

```brew install opam```

Create the opam environment

```opam init --comp 4.04.2```

Then we need the actual dependencies for Ocaml.

```opam install llvm.3.8 core.v0.9.1 ounit ctypes-foreign utop dolog menhir oasis ppx_deriving ANSITerminal ocamlgraph```

Initialize the opam environment.

```eval $(opam config env)```

In the FaCT directory, set up oasis.

```oasis setup```

Then we must configure oasis.

```make configure```

Finally we can build the compiler.

```make build```

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
