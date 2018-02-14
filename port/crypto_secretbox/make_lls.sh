#!/bin/bash

../../fact.byte -opt O0 -ast-out -pseudocode -generate-header -llvm-out -debug stdlib.fact crypto_poly1305.fact crypto_secretbox.fact
cp crypto_secretbox.ll crypto_secretbox.0.ll
../../fact.byte -opt O1 -ast-out -pseudocode -generate-header -llvm-out -debug stdlib.fact crypto_poly1305.fact crypto_secretbox.fact
cp crypto_secretbox.ll crypto_secretbox.1.ll
../../fact.byte -opt O2 -ast-out -pseudocode -generate-header -llvm-out -debug stdlib.fact crypto_poly1305.fact crypto_secretbox.fact
cp crypto_secretbox.ll crypto_secretbox.2.ll
