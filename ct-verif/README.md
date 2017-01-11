# Testing with ct-verif

### Setting up 
1. The verifying-constant-time directory is a [submodule](https://git-scm.com/book/en/v2/Git-Tools-Submodules) that will contain ct-verif, which can be cloned via:
```sh
$ git submodule init
$ git submodule update
```
2. Next, build ct-verif. This will require `sudo` privileges in order to ensure the right version of LLVM is installed/used:
```sh
$ cd verifying-constant-time/bin/
$ sudo ./provision.sh 
```
3. Examples of .c files using ct-verif are provided in `verifying-constant-time/examples`.
4. TODO: More
