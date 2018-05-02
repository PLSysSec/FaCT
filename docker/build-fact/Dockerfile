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
RUN echo "root:root" | chpasswd
RUN echo "docker:docker" | chpasswd
RUN usermod -aG sudo docker

USER root
RUN apt-get update && \
    apt-get -y install apt-transport-https ca-certificates curl gnupg2 software-properties-common && \
    curl -fsSL https://download.docker.com/linux/$(. /etc/os-release; echo "$ID")/gpg > /tmp/dkey; apt-key add /tmp/dkey && \
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/$(. /etc/os-release; echo "$ID") $(lsb_release -cs) stable" && \
    apt-get update && \
    apt-get -y install docker-ce

###############################

COPY user-mapping-new.sh /user-mapping.sh
RUN  chmod u+x /user-mapping.sh

###############################

USER root
RUN apt-get -y purge clang-3.8 llvm-3.8 
RUN apt-get -y update && apt-get -y upgrade

RUN echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" | tee -a /etc/apt/sources.list
RUN echo "deb-src http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" | tee -a /etc/apt/sources.list
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
RUN apt-get update
RUN apt-get -y install clang-6.0 lldb-6.0 lld-6.0
RUN apt-get -y autoremove
RUN apt-get -y install cmake

USER docker
RUN cd /home/docker/FaCT && opam update && opam upgrade
RUN cd /home/docker/FaCT && opam install llvm.6.0.0 
RUN echo "export PATH=/usr/lib/llvm-6.0/bin/:$PATH" | tee -a /home/docker/.profile

USER root


