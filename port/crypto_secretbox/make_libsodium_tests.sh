#!/bin/bash
set -e

# IMPORTANT this file assumes that it is being run from the libsodium root directory
# also it assumes that you've already built the object files in port/crypto_secretbox

LIBSODIUM=$PWD
FACT_DIR=~/const/fact
OBJ_DIR=$FACT_DIR/port/crypto_secretbox

ASM=no
BENCHMARKS=
NO_FACT=
TRACE_IMPL=
opts=$(getopt -- 'abnt' "$@")
eval set -- $opts
while [[ $# > 0 ]]; do
  case "$1" in
    -a )
      ASM=yes
      shift 1;;
    -b )
      BENCHMARKS=yes
      shift 1;;
    -n )
      NO_FACT=yes
      shift 1;;
    -t )
      TRACE_IMPL=yes
      shift 1;;
    -- )
      shift
      break;;
  esac
done

cd $LIBSODIUM/src/libsodium/include/sodium/private
git checkout -- common.h
git checkout -- ../export.h
if [[ -n $TRACE_IMPL ]]; then
  sed -i -e '/#define common_H/a#ifndef TRACE\n#define TRACE(s) { static int __ = 1; if (__) { puts(s); __ = 0; } }\n#endif' common.h
  sed -i -e '/#define sodium_export_H/a#ifndef TRACE\n#define TRACE(s) { static int __ = 1; if (__) { puts(s); __ = 0; } }\n#endif' ../export.h
else
  sed -i -e '/#define common_H/a#ifndef TRACE\n#define TRACE(s) do { } while (0)\n#endif' common.h
  sed -i -e '/#define sodium_export_H/a#ifndef TRACE \n#define TRACE(s) do { } while (0)\n#endif' ../export.h
fi

cd $LIBSODIUM
CC=clang-3.8 ./configure --enable-asm=$ASM

echo > $LIBSODIUM/src/libsodium/include/sodium/fact_secretbox.h
if [[ -z $NO_FACT ]]; then
  # (re)compile FaCT port
  cd $OBJ_DIR
  # using fifos to send commands to docker instance
  # docker needs to be running `while true ; do bash < dock ; done`
  echo make clean > dock
  echo make "FFLAGS='-opt O2'" crypto_secretbox.o > dock
  echo 'echo > dockout' > dock
  echo "Waiting on docker..."
  cat dockout
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

git checkout -- cmptest.h
for box in secretbox{,2,7,8}; do
  touch $box.c
  make $box.log
  cat $box.log
done

if [[ -n $BENCHMARKS ]]; then
  sed -i -e '/#define __CMPTEST_H__/a#define BENCHMARKS' cmptest.h
  grep -q -e '#  define ITERATIONS 128' cmptest.h
  sed -i -e 's/#  define ITERATIONS 128/#  define ITERATIONS (0x1000000)/' cmptest.h
  sed -i -e '/int main(void)/istatic void prcomma(unsigned long long diff) \{\n  if (diff < 1000) \{\n    printf("%llu", diff);\n    return;\n  \}\n  prcomma(diff / 1000);\n  printf(",%03llu", diff % 1000);\n\}\n' cmptest.h
  sed -i -e 's/.*printf.*ITERATIONS.*/    prcomma(1000000ULL * (ts_end - ts_start) \/ ITERATIONS);\n    printf("\\n");/' cmptest.h
  sed -i -e '/#undef  printf/i#undef  assert\n#define assert(x) do { } while(0)' cmptest.h
  echo >> ../../benchmarks.log
  date >> ../../benchmarks.log
  if [[ -z $NO_FACT ]]; then
    echo "FaCT implementation" >> ../../benchmarks.log
  else
    echo "C implementation, asm: $ASM" >> ../../benchmarks.log
  fi
  for box in secretbox{,2}; do
    touch $box.c
    echo -n "Benchmark ($box): " | tee -a ../../benchmarks.log
    make $box.log >/dev/null
    echo $(grep -E '^[[:digit:],]+$' $box.log) "picoseconds" | tee -a ../../benchmarks.log
  done
fi
