FROM ubuntu:16.04
RUN apt-get -y update
RUN apt-get -y install apt-utils
RUN apt-get -y install git wget sudo software-properties-common lsb-release
RUN apt-get -y install mono-complete

# build SMACK (with multi-language support PR)
RUN cd /root && git clone https://github.com/smackers/smack.git -b develop
RUN cd /root/smack/bin && ./build.sh
ENV BOOGIE="mono /root/boogie/Binaries/Boogie.exe"
ENV CORRAL="mono /root/corral/bin/Release/corral.exe"

# install Bam
RUN apt-get -y install ruby-dev
RUN gem install bam-bam-boogieman bundler

# install ctverif
RUN apt-get -y install npm nodejs-legacy
RUN npm install -g n && n lts
RUN npm i -g ctverif

# setup directory for running ctverif
RUN mkdir /root/verifs
