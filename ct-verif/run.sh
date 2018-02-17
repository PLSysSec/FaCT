#!/bin/bash

docker cp sort.ll ctverif_cont:/root/fact-verifs
docker exec ctverif_cont /bin/bash -c 'cd /root/fact-verifs && ENTRYPOINTS=sort3_wrapper FACTLL=sort.ll make && make clean'
