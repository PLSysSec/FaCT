# Docker with FaCT
There is an image on Docker Cloud with everything installed that is needed to use the FaCT compiler. For development it may be best to modify files on the host, and compile/etc. on the Docker container. To use the image, run:

```./run.sh```

This will download and save the image (warning: 900 MB compressed) then open a shell into the container. It mounts the parent FaCT directory from the host machine to `/home/docker/FaCT` in the container. To get started, build the compiler in the container:

```
cd /home/docker/FaCT
eval $(opam config env)
oasis setup
make
```

This will produce `fact.byte`, the FaCT executable. If you're getting errors try running `make clean` and removing `setup.data` then running the above commands again.

Additionally, to verify code as constant-time, the FaCT compiler uses [ct-verif](https://github.com/imdea-software/verifying-constant-time/) in a Docker container (warning: compressed image is 720 MB). It can be used regardless of whether the FaCT container is used.
