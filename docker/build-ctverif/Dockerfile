FROM ubuntu:14.04
RUN apt-get -y update 
RUN apt-get -y install git cmake python-yaml unzip wget clang-3.5 llvm-3.5 mono-complete libz-dev libedit-dev binutils software-properties-common
RUN cd /root && git clone https://github.com/PLSysSec/verifying-constant-time.git -b oldversion && cd /root/verifying-constant-time/bin && echo | ./provision.sh
RUN cd /root/verifying-constant-time/bin && make local-tools

RUN mkdir /root/fact-verifs
COPY Makefile-ll /root/fact-verifs/Makefile

RUN mkdir /root/c-verifs
COPY Makefile-c /root/c-verifs/Makefile
