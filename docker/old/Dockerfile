FROM ubuntu:16.04
RUN apt-get -y update && \
    apt-get -y install sudo ocaml llvm-3.8 opam m4 pkg-config clang-3.8 clang llvm ruby-full

####################################################
# from https://gist.github.com/renzok/29c9e5744f1dffa392cf
ENV USER=docker USER_ID=1000 USER_GID=1000

# now creating user
RUN groupadd --gid "${USER_GID}" "${USER}" && \
    useradd \
      --uid ${USER_ID} \
      --gid ${USER_GID} \
      --create-home \
      --shell /bin/bash \
      ${USER}

COPY user-mapping.sh /
RUN  chmod u+x /user-mapping.sh

ENTRYPOINT ["/user-mapping.sh"]
####################################################

# setup FaCT
USER ${USER}
RUN mkdir /home/docker/FaCT && cd /home/docker/FaCT && \
    opam init --compiler=4.03.0 && \
    opam install llvm.3.8 core ounit ctypes-foreign utop dolog menhir oasis ppx_deriving ANSITerminal ocamlgraph

USER root

