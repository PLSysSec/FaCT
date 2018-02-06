#!/bin/bash 

docker run -it -v $(pwd)/..:/home/docker/FaCT --rm -e HOST_USER_ID=$(id -u) -e HOST_USER_GID=$(id -g) bjohannesmeyer/fact /bin/bash

