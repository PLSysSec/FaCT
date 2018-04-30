#!/bin/bash

DOCKERIMG=bjohannesmeyer/ctverif

if [ "$#" -ne 4 ] || [ ! -e $1 ] || [ ! -e $2 ] || [ ! -e $3 ]; then 
    echo "Usage: $0 ctvwrapper.c fact.h fact-3.8.ll entrypoint" ; exit 1
fi

CTVWRAPPERCPATH=$1
CTVWRAPPERC=$(basename $CTVWRAPPERCPATH)

FACTLIBPATH=$2
FACTLIB=$(basename $FACTLIBPATH)

FACT38LLPATH=$3
FACTLLPATH=${FACT38LLPATH}-3.5.ll
FACTLL=$(basename $FACTLLPATH)
cp $FACT38LLPATH $FACTLLPATH

ENTRYPOINTS=$4

## Jankily convert a 3.8 LLVM .ll file into a 3.5 LLVM .ll file
# Find `load i32,`; replace with `load`
# Find `getelementptr [2 x i32],`; replace with `getelementptr`
# Find `smack_value* (i32*, ...) bitcast`; replace with `smack_value* (i32*, ...)* bitcast`
# Remove lines with `!0`
# Find `norecurse`; replace with ``
# Find `argmemonly`; replace with ``
sed -i -e 's/load[^,]*, /load /g' $FACTLLPATH
sed -i -e 's/getelementptr i64, /getelementptr /g' $FACTLLPATH
sed -i -e 's/getelementptr inbounds [^,]*, /getelementptr inbounds /g' $FACTLLPATH
sed -i -e 's/smack_value\(.*\)) bitcast/smack_value\1)* bitcast/g' $FACTLLPATH
sed -i.bak '/!0/d' $FACTLLPATH && rm ${FACTLLPATH}.bak
sed -i -e 's/norecurse//g' $FACTLLPATH
sed -i -e 's/argmemonly//g' $FACTLLPATH

# Copy into docker container and verify
docker pull $DOCKERIMG 
ID=$(docker run -it -d --rm $DOCKERIMG /bin/bash)
trap 'docker stop $ID ; exit ' 2 # if receive SIGINT, kill the docker container
docker cp $CTVWRAPPERCPATH $ID:/root/fact-verifs/
docker cp $FACTLIBPATH $ID:/root/fact-verifs/
docker cp $FACTLLPATH $ID:/root/fact-verifs/
docker exec $ID /bin/bash -c "cd /root/fact-verifs && CTVWRAPPERC=$CTVWRAPPERC FACTLIB=$FACTLIB FACTLL=$FACTLL ENTRYPOINTS=$ENTRYPOINTS make && make clean"
docker stop $ID

