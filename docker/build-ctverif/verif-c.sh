#!/bin/bash

if [ "$#" -ne 3 ] || [ ! -e $1 ] || [ ! -e $2 ]; then 
    echo "Usage: $0 [C file] [C wrapper] [entrypoint]" ; exit 1
fi

# Copy into docker container and verify
docker run -it -d --rm --name ctverif_cont bjohannesmeyer/ctverif /bin/bash
docker cp $1 ctverif_cont:/root/c-verifs
docker cp $2 ctverif_cont:/root/c-verifs
docker exec ctverif_cont /bin/bash -c "cd /root/c-verifs && LIBS=$1 EXAMPLE=$2 ENTRYPOINTS=$3 make && make clean"
docker stop ctverif_cont

