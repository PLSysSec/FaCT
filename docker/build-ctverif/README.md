# ct-verif with FaCT

There is an image on Docker Cloud at bjohannesmeyer/ctverif with ct-verif installed. Pass the `-verify-llvm` flag to the FaCT compiler to verify code with ct-verif. 



The `verif.sh` script takes a function entrypoint and files to verify:

```
./verif.sh [entrypoint] [files ...]
```



For example, 

```./verif.sh foo_wrapper example/foo.h example/foo.c example/foo_wrapper.c```

 verifies the `foo_wrapper` function. Any file type that SMACK can handle can be passed; for example, `example/foo.ll` could be passed instead of `example/foo.c`.



If you're running into problems, it may be because you have a previous version of the docker image. In that case, try running `docker pull bjohannesmeyer/ctverif`.


