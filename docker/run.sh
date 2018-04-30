#!/bin/bash 

docker pull bjohannesmeyer/fact
docker run -it --rm \
    -v $(pwd)/..:/home/docker/FaCT \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -e HOST_USER_ID=$(id -u) -e HOST_USER_GID=$(id -g) \
    bjohannesmeyer/fact /bin/bash

