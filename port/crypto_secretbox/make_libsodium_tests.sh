#!/bin/bash
set -e

# IMPORTANT this file assumes that it is being run from the libsodium root directory
# also it assumes that you've already built the object files in port/crypto_secretbox

LIBSODIUM=$PWD
FACT_DIR=~/research/const/constc
OBJ_DIR=$FACT_DIR/port/crypto_secretbox

BENCHMARKS=
NO_FACT=
eval set -- $(getopt -- 'bn' "$@")
while [[ $# > 0 ]]; do
  case "$1" in
    -b )
      BENCHMARKS=yes
      shift 1;;
    -n )
      NO_FACT=yes
      shift 1;;
    -- )
      shift
      break;;
  esac
done

echo > $LIBSODIUM/src/libsodium/include/sodium/fact_secretbox.h
if [[ -z $NO_FACT ]]; then
  # (re)compile FaCT port
  cd $OBJ_DIR
  make clean
  make FFLAGS='-opt O2' crypto_secretbox.o
    # this is for doing custom builds off of modified .ll
    #   llc -O2 crypto_secretbox.ll
    #   clang -O2 -c crypto_secretbox.s
    #   llc -O2 -relocation-model=pic crypto_secretbox.ll -o crypto_secretbox.fpic.s
    #   clang -O2 -c crypto_secretbox.fpic.s -o crypto_secretbox.fpic.o
  cp $OBJ_DIR/crypto_secretbox.h $LIBSODIUM/src/libsodium/include/sodium/fact_secretbox.h

  # (re)compile with fact stubbed in
  cd $LIBSODIUM/src/libsodium
  touch crypto_secretbox/crypto_secretbox.c
  rm -f crypto_secretbox/*.lo
  rm -rf crypto_secretbox/.libs
  touch crypto_core/salsa/ref/core_salsa_ref.c
  touch crypto_core/hsalsa20/ref2/core_hsalsa20_ref2.c
  touch crypto_stream/salsa20/stream_salsa20.c
  cd $LIBSODIUM
  make -j4

  cd $LIBSODIUM/src/libsodium/crypto_secretbox
  # copy the object files
  cp $OBJ_DIR/crypto_secretbox{,.fpic}.o .
  cp libsodium_la-crypto_secretbox.o secretbox.o
  ld -r secretbox.o crypto_secretbox.o -o libsodium_la-crypto_secretbox.o

  # libsodium hides more object files in hidden directories
  cd .libs
  cp libsodium_la-crypto_secretbox.o secretbox.o
  ld -r secretbox.o ../crypto_secretbox.fpic.o -o libsodium_la-crypto_secretbox.o

  # build libsodium with new object files
  cd $LIBSODIUM
  touch src/libsodium/crypto_secretbox/libsodium_la-crypto_secretbox.lo
  make -j4
else
  # force recompile of libsodium secretbox
  cd $LIBSODIUM/src/libsodium/crypto_secretbox
  touch crypto_secretbox.c
  cd $LIBSODIUM
  make -j4
fi

cd test/default
for box in secretbox{,2,7,8}; do
  cp cmptest.h.template cmptest.h
  touch $box.c
  make $box.log
  cat $box.log
  if [[ -n $BENCHMARKS && $box != "secretbox7" && $box != "secretbox8" ]]; then
    sed -e '4i#define BENCHMARKS' cmptest.h.template > cmptest.h
    touch $box.c
    make $box.log >/dev/null
    head -n1 $box.log
  fi
done
