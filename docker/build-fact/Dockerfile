FROM ubuntu:16.04
RUN apt-get -y update
RUN apt-get -y install sudo ocaml opam m4 pkg-config ruby-full

# install llvm6
RUN echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" | tee -a /etc/apt/sources.list
RUN echo "deb-src http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" | tee -a /etc/apt/sources.list
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
RUN apt-get -y update
RUN apt-get -y install clang-6.0 lldb-6.0 lld-6.0 cmake

####################################################
# add a "docker" user
ENV USER=docker USER_ID=1000 USER_GID=1000

RUN groupadd --gid "${USER_GID}" "${USER}" && \
    useradd \
      --uid ${USER_ID} \
      --gid ${USER_GID} \
      --create-home \
      --shell /bin/bash \
      ${USER}

COPY user-mapping.sh /user-mapping.sh
RUN  chmod u+x /user-mapping.sh

ENTRYPOINT ["/user-mapping.sh"]

RUN echo "root:root" | chpasswd
RUN echo "docker:docker" | chpasswd
RUN usermod -aG sudo docker

####################################################
# install docker
RUN apt-get -y install apt-transport-https ca-certificates curl gnupg2 software-properties-common && \
    curl -fsSL https://download.docker.com/linux/$(. /etc/os-release; echo "$ID")/gpg > /tmp/dkey; apt-key add /tmp/dkey && \
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/$(. /etc/os-release; echo "$ID") $(lsb_release -cs) stable" && \
    apt-get update && \
    apt-get -y install docker-ce

####################################################
# setup user's account
USER ${USER}
RUN echo "export PATH=/usr/lib/llvm-6.0/bin/:$PATH" | tee -a /home/docker/.profile
RUN opam init --compiler=4.06.0 
RUN cd /home/docker/ && wget https://raw.githubusercontent.com/PLSysSec/FaCT/rewrite/ocamlswitch.txt && opam switch import ocamlswitch.txt && rm ocamlswitch.txt
RUN echo "export LD_LIBRARY_PATH=$HOME/.opam/4.06.0/lib/z3" | tee -a /home/docker/.profile 
RUN mkdir /home/docker/FaCT

###############################

USER root


