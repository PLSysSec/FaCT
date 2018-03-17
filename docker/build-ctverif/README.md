# ct-verif with FaCT
There is an image on Docker Cloud at bjohannesmeyer/ctverif with ct-verif installed. Pass the `-verify-llvm` flag to the FaCT compiler to verify code with ct-verif. 

The ct-verif container takes LLVM 3.5 .ll files as input, or 3.8 files that have been converted into 3.5. For example, to verify `sort.ll` (made from sort.c in the ct-verif repo), run:

```./verif.sh examples/sort-3.8.ll sort3_wrapper```


