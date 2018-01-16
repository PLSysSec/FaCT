# Docker with FaCT
There is an image on Docker Cloud at bjohannesmeyer/fact with everything installed that is needed to use the FaCT compiler. For development it may be best to modify files on the host, and compile/etc. on the Docker container. To use the image, run:

```run.sh```

This will download and save the image (warning: 750 MB) then open a shell into an instance of it (i.e., a container). It mounts the parent FaCT directory from the host machine to `/home/docker/FaCT` in the container. To get started, build the compiler in the container:

```
cd /home/docker/FaCT
eval $(opam config env)
oasis setup
make configure
make build
```

This will produce `fact.byte`, the FaCT executable.
