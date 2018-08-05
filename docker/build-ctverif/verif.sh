#!/bin/bash

if [ "$#" -lt 2 ]; then 
    echo "Usage: $0 [entrypoint] [files ...]" ; exit 1
fi
ENTRYPOINT=$1

for ((i=2; i<=$#; ++i)); do
    if [ ! -f ${!i} ]; then
        echo "File ${!i} not found!"
        exit 0
    fi
done

# Start docker container
ID=$(docker run -it -d --rm new_ctverif_img /bin/bash)

# Copy files into container (apparently `docker cp` can't handle multiple files)
for ((i=2; i<=$#; ++i)); do
    docker cp ${!i} $ID:/root/verifs/
done

# Run ctverif (excluding header files from args)
# This will probably break with any relative paths to header files that aren't in the local directory
docker exec $ID /bin/bash -c "ctverif --entry-points $ENTRYPOINT \$(find /root/verifs/ -type f ! \( -iname '*.h' \))"

# Stop container
docker stop $ID
