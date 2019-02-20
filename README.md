
# FaCT

This is the compiler for the Flexible and Constant Time cryptographic programming language.
FaCT is a domain-specific language that aids you in writing constant-time code for cryptographic routines
that need to be free from timing side channels.

## Useful links:

- Online demo: http://fact.sysnet.ucsd.edu:10240/ (thanks to https://github.com/mattgodbolt/compiler-explorer)
- Sample FaCT code: https://github.com/PLSysSec/fact-eval
- Haskell embedding: https://github.com/PLSysSec/inline-fact
- Python embedding: https://github.com/PLSysSec/CTFFI
- Vim syntax files: https://github.com/PLSysSec/factlang.vim

## Usage

#### Basic Usage

Run ```./factc ex.fact``` to compile a FaCT program where ```ex.fact``` is the name of your file.

#### Link to a C library

FaCT is designed to be called from C code. In order to do so, write your FaCT functions and compile them. This will output an object file. This can then be linked to a C file. For example:

```./factc ex.fact```

Then we compile the calling C file:

```
clang -c main.c
```

Next, we link them together:

```clang -o final main.o ex.o```

Finally, we can run the executable:

```./final```

#### Debugging

Many debugging options and intermediate data structures are available. Run ```./factc -help``` for all options.

## Installation

To install you can either build the source or download ```factc```. We recommend building from source if possible.

### Setting up the build environment

FaCT is developed using Ocaml and LLVM 6.0.

You use a docker image with an environment already set up to build the compiler:

```
cd docker/
./run.sh
# run the following once inside the docker shell:
eval $(opam config env)
```

Otherwise, you can set up a build environment locally, following the directions below.

We recommend installing Ocaml via the opam package manager:

```sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)```

The FaCT compiler depends on the LLVM 6.0 toochain,
and expects binaries with `-6.0` suffixes:

```sudo apt install llvm-6.0 clang-6.0```

On OS X it can be done with brew.

```
brew install opam
brew install llvm60
```

If the toolchain is not installed with `-6.0` suffixes on the binaries,
you will need to add symlinks in one of your PATH directories so
that `clang-6.0` exists in the PATH and points to the Clang 6.0 compiler.

Then we need to install Ocaml and the necessary libraries.
If you do not already have the following packages installed, install them now:
```sudo apt install cmake libgmp-dev m4 pkg-config```

Then, install Ocaml and the libraries:

```
opam init
eval $(opam config env)
opam switch create 4.06.0
eval $(opam config env)
opam switch import ocamlswitch.txt
```

Finally, make sure the Z3 lib is in the path:

```export LD_LIBRARY_PATH="$HOME/.opam/4.06.0/lib/z3"```

### Compiling FaCT

If you have not setup oasis, then you must do that first.

```oasis setup```

We can now build the compiler:

```make```

This will produce the ```factc``` executable.
