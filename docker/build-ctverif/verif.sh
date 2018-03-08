#!/bin/bash

if [ "$#" -ne 2 ] || [ ! -e $1 ]; then 
    echo "Usage: $0 llvm3.8.ll entrypoint" ; exit 1
fi
cp $1 tmp.ll

## Jankily convert a 3.8 LLVM .ll file into a 3.5 LLVM .ll file
# Find `load i32,`; replace with `load`
# Find `getelementptr [2 x i32],`; replace with `getelementptr`
# Find `smack_value* (i32*, ...) bitcast`; replace with `smack_value* (i32*, ...)* bitcast`
# Remove lines with `!0`
# Find `norecurse`; replace with ``
sed -i -e 's/load[^,]*, /load /g' tmp.ll
sed -i -e 's/getelementptr inbounds [^,]*, /getelementptr inbounds /g' tmp.ll 
sed -i -e 's/smack_value\(.*\)) bitcast/smack_value\1)* bitcast/g' tmp.ll 
sed -i '/!0/d' tmp.ll
sed -i -e 's/norecurse//g' tmp.ll

# Copy into docker container and verify
docker run -it -d --rm --name ctverif_cont bjohannesmeyer/ctverif /bin/bash
docker cp tmp.ll ctverif_cont:/root/fact-verifs
rm tmp.ll
docker exec ctverif_cont /bin/bash -c "cd /root/fact-verifs && ENTRYPOINTS=$2 FACTLL=tmp.ll make && make clean && rm tmp.ll"
docker stop ctverif_cont

