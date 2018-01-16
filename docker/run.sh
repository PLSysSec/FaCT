#!/bin/bash 

sudo docker run -it -v $(pwd)/..:/home/docker/FaCT --rm -e HOST_USER_ID=$(id -u) -e HOST_USER_GID=$(id -g) fact_img bash

