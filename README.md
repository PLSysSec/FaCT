
# FaCT

This is the compiler for the Flexible and Constant Time cryptographic programming language.
FaCT is a domain-specific language that aids you in writing constant-time code for cryptographic routines
that need to be free from timing side channels.

## Useful links:

- [Our paper](/FaCT_extended.pdf)
- [Online demo using Compiler Explorer](https://fact.sysnet.ucsd.edu/) ([source](https://github.com/bjohannesmeyer/fact-website) forked from [mattgodbolt/compiler-explorer](https://github.com/mattgodbolt/compiler-explorer))
- [FaCT case studies / evaluation](https://github.com/PLSysSec/fact-eval)
- [Haskell embedding](https://github.com/PLSysSec/inline-fact)
- [Python embedding](https://github.com/PLSysSec/CTFFI)
- [Vim syntax files](https://github.com/PLSysSec/factlang.vim)

## Building

To build the compiler, you can either build from source or download a pre-built release.
We recommend building from source if possible.

### Virtual machine image

You can download a [VM image pre-configured for building the FaCT compiler and case studies](https://drive.google.com/open?id=1xzw4Ohsdj4WqxJPl1RvvxSnhysMSejPi).
The file `fact.ova` should have a SHA256 sum of `089398c85c5074d911c2f2b67ca22df453235e8733f1eb283c71717cf70f714c`.

### Using Docker

If you have docker installed, you can load our docker image with the build
environment already installed:

```
cd docker/
./run.sh
```

Once inside the docker shell, run the following to finish setting up the environment:

```
cd FaCT/
eval $(opam config env)
```

### Setting up the build environment

FaCT is developed using OCaml 4.06.0 and LLVM 6.0.

### Using a local environment

Building FaCT has been tested on Ubuntu 16.04, 18.04, and macOS.


#### 1. Install System Dependencies

**Ubuntu (16.04 & 18.04)**


```
sudo apt install llvm-6.0 clang-6.0 cmake libgmp-dev m4 pkg-config
```

**macOS**

These instructions require [Homebrew](https://brew.sh)
```
brew install cmake gmp m4 pkg-config
brew install llvm@6 --with-toolchain
```

**Note:** This does not put the proper version of clang on your `PATH`. You will need to run:
```
export PATH="$(brew --prefix llvm@6)/bin:$PATH"
```

#### 2. Install OCaml + packages

We recommend installing the [opam package manager](https://opam.ocaml.org/) to manage OCaml and package dependencies:

```
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```

Then, install OCaml and the libraries:

```
opam init
eval $(opam config env)
opam switch create 4.06.0
eval $(opam config env)
opam switch import ocamlswitch.txt
```

Finally, make sure the Z3 lib is available to the OCaml compiler:
```
export LD_LIBRARY_PATH="$HOME/.opam/4.06.0/lib/z3:$LD_LIBRARY_PATH"
```

#### 3. Configure Paths
The FaCT compiler depends on the LLVM 6.0 toolchain **at runtime**,
and expects binaries with `-6.0` suffixes. 
Ensure that `clang-6.0` is in your PATH:

```
clang-6.0 --version
```

### Compiling FaCT

You can now build the compiler:

```
oasis setup
make
```

This will produce the `factc` executable.

## Usage

#### Basic Usage

Run ```./factc <source files>``` to compile a FaCT program.

#### Link to a C library

FaCT is designed to be called from C code. Compiling FaCT source files will
output an object file, which can then be linked to a C file. As an example:

```
cd example/
../factc -generate-header example.fact
clang-6.0 -c main.c
clang-6.0 -o final main.o example.o
```

You can then run the executable:

```./final```

#### Debugging

Many debugging options and intermediate data structures are available. Run ```./factc -help``` for all options.

## Acknowledgements

We thank the anonymous PLDI and PLDI AEC reviewers for their suggestions and
insightful comments.
