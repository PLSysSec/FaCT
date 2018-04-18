#!/bin/bash

if [ "$#" -ne 3 ] || [ ! -e $1 ] || [ ! -e $2 ]; then 
    echo "Usage: $0 [C file] [C wrapper] [entrypoint]" ; exit 1
fi

CPATH=$1
CFILE=$(basename $CPATH)
WRAPPERPATH=$2
WRAPPERFILE=$(basename $WRAPPERPATH)

# Copy into docker container and verify
ID=$(docker run -it -d --rm bjohannesmeyer/ctverif /bin/bash)
docker cp $CPATH       $ID:/root/c-verifs/$CFILE
docker cp $WRAPPERPATH $ID:/root/c-verifs/$WRAPPERFILE
docker exec $ID /bin/bash -c "cd /root/c-verifs && LIBS=$CFILE EXAMPLE=$WRAPPERFILE ENTRYPOINTS=$3 make && make clean"
docker stop $ID
