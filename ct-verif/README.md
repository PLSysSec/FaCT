# Testing with ct-verif

### Set up 
The verifying-constant-time directory is a [submodule](https://git-scm.com/book/en/v2/Git-Tools-Submodules) that will contain a fork of ct-verif that can process .bc files. This can be cloned via:
```sh
$ git submodule init
$ git submodule update
```

### Build
Next, build ct-verif. `sudo` privileges are required for the provisioning script in order to ensure the right version of LLVM is installed/used:
```sh
$ cd verifying-constant-time/bin/
$ ./provision.sh 
```

### Verify
Next, run examples through ct-verif. There are currently two examples of how to verify the OpenSSL function with ct-verif in `ct-verif/tests/`. `openssl-c` has it as C code, and `openssl-bc` has it as LLVM bitcode produced from Constanc. Both use a C wrapper. For both, simply go into the directory and run `make` to verify. To create a new test directory, copy one of the sample directory's `Makefile`s and change the first 3 lines (i.e. `ENTRYPOINTS`, `INPUTFILE`, `WRAPPERFILE`) to fit your test. More examples of C files using ct-verif can be found in their examples directory in `ct-verif/verifying-constant-time/examples`

### LLVM version compliance
To convert LLVM 3.9 .ll files (the kind produced by Constanc) into LLVM 3.5 .ll files (the kind inputted into ct-verif), there are a few instructions that need to be replaced. There may be more in the future, but for now, replacing these fixes it:
- Find `load i32,`; replace with `load`
- Find `getelementptr [2 x i32],`; replace with `getelementptr`
