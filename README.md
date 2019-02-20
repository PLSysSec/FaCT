
# FaCT

This is the compiler for the Flexible and Constant Time cryptographic programming language.
FaCT is a domain-specific language that aids you in writing constant-time code for cryptographic routines
that need to be free from timing side channels.

## Useful links:

- Online demo: http://fact.sysnet.ucsd.edu:10240/ (thanks to https://github.com/mattgodbolt/compiler-explorer)
- FaCT case studies / evaluation: https://github.com/PLSysSec/fact-eval
- Haskell embedding: https://github.com/PLSysSec/inline-fact
- Python embedding: https://github.com/PLSysSec/CTFFI
- Vim syntax files: https://github.com/PLSysSec/factlang.vim

## Usage

#### Basic Usage

Run ```./factc <source files>``` to compile a FaCT program.

#### Link to a C library

FaCT is designed to be called from C code. Compiling FaCT source files will
output an object file, which can then be linked to a C file. For example, if
your FaCT functions are in `ex.fact` and your C program is in `main.c`:

```
./factc ex.fact
clang -c main.c
clang -o final main.o ex.o
```

You can then run the executable:

```./final```

#### Debugging

Many debugging options and intermediate data structures are available. Run ```./factc -help``` for all options.

## Building

To build the compiler, you can either build from source or download a pre-built release.
We recommend building from source if possible.

### Setting up the build environment

FaCT is developed using OCaml and LLVM 6.0.

You may use a docker image with an environment already set up to build the compiler:

```
cd docker/
./run.sh
# run the following once inside the docker shell:
eval $(opam config env)
```

Otherwise, you can set up a build environment locally. The directions below
have been tested on Ubuntu 16.04 and 18.04.

We recommend installing OCaml via the opam package manager:

```sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)```

The FaCT compiler depends on the LLVM 6.0 toochain,
and expects binaries with `-6.0` suffixes:

```sudo apt install llvm-6.0 clang-6.0```

Ensure that `clang-6.0` is in your PATH:

```clang-6.0 --version```

Install OCaml and the necessary libraries.
If you do not already have the following packages installed, install them now:
```sudo apt install cmake libgmp-dev m4 pkg-config```

Then, install OCaml and the libraries:

```
opam init
eval $(opam config env)
opam switch create 4.06.0
eval $(opam config env)
opam switch import ocamlswitch.txt
```

Finally, make sure the Z3 lib is available to the OCaml compiler:

```export LD_LIBRARY_PATH="$HOME/.opam/4.06.0/lib/z3:$LD_LIBRARY_PATH"```

### Compiling FaCT

You can now build the compiler:

```
oasis setup
make
```

This will produce the ```factc``` executable.
