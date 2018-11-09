#!/bin/bash

usage() {
    echo "Usage: $0 [-v for verbose or -q for quiet] [entrypoint] [files ...]" 
    exit 1
}

# Run ctverif (excluding header files from args)
# This will probably break with any relative paths to header files that aren't in the local directory
run_ctverif() {
    docker exec $1 /bin/bash -c "ctverif --unroll 5 --entry-points $2 \$(find /root/verifs/ -type f ! \( -iname '*.h' \)) 2>&1"
}

if [ "$#" -lt 3 ]; then usage $0; fi

if [ $1 = "-v" ]; then VERBOSE=true
elif [ $1 = "-q" ]; then VERBOSE=false
else usage $0; fi

ENTRYPOINT=$2

for ((i=3; i<=$#; ++i)); do
    if [ ! -f ${!i} ]; then
        echo "File ${!i} not found!"
        exit 1
    fi
done

# Start docker container
ID=$(docker run -it -d --rm bjohannesmeyer/ctverif /bin/bash)

# Copy files into container (apparently `docker cp` can't handle multiple files)
for ((i=3; i<=$#; ++i)); do
    docker cp ${!i} $ID:/root/verifs/
done


# Run ctverif and fail if "SMACK found no errors with unroll bound" is not in output
# If verbose, print everything out as ctverif runs
if [ $VERBOSE = true ]; then
    OUTP=$(run_ctverif $ID $ENTRYPOINT | tee /dev/tty)
else
    OUTP=$(run_ctverif $ID $ENTRYPOINT)

fi

# Stop container
docker stop $ID > /dev/null

# If ctverif failed --- i.e. "SMACK found no errors with unroll bound" is not in output --- then exit with an error
# If verbose, then also print ctverif's output
if ! echo "$OUTP" | grep "SMACK found no errors with unroll bound" > /dev/null ; then
    if [ $VERBOSE = false ]; then echo "$OUTP"; fi
    exit 1
fi

echo "SMACK found no errors"
