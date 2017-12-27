#!/bin/bash

# IMPORTANT this file assumes that it is being run from the libsodium root directory
# also it assumes that you've already built the object files in port/crypto_secretbox

FACT_DIR=~/research/const/constc
OBJ_DIR=$FACT_DIR/port/crypto_secretbox

touch src/libsodium/crypto_secretbox/crypto_secretbox.c
make -j4

cd src/libsodium/crypto_secretbox
cp $OBJ_DIR/*.o .
cp libsodium_la-crypto_secretbox.o secretbox.o
ld -r secretbox.o crypto_secretbox.o crypto_secretbox_wrapper.o -o libsodium_la-crypto_secretbox.o

cd .libs
cp libsodium_la-crypto_secretbox.o secretbox.o
ld -r secretbox.o ../crypto_secretbox.fpic.o ../crypto_secretbox_wrapper.fpic.o -o libsodium_la-crypto_secretbox.o

cd ../../../../
touch src/libsodium/crypto_secretbox/libsodium_la-crypto_secretbox.lo
make -j4

cd test/default
make -j4 recheck
