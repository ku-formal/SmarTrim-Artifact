FROM ubuntu:22.04

WORKDIR /root/

### Install packages and utilities

# You may replace the URL for into a faster one in your region.
# RUN sed -i 's/archive.ubuntu.com/ftp.daumkakao.com/g' /etc/apt/sources.list
ENV DEBIAN_FRONTEND="noninteractive"

RUN apt-get update && \
    apt-get -yy install \
      wget apt-transport-https git unzip \
      build-essential libtool libtool-bin \
      automake autoconf bison flex sudo \
      curl software-properties-common \
      apt-utils locales \
      pandoc \
      locales-all
RUN locale-gen en_US.UTF-8

# Install .NET Core
RUN wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    apt-get update && apt-get -yy install dotnet-sdk-8.0 && \
    rm -f packages-microsoft-prod.deb
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1

WORKDIR /root

# badd4ff is currently the latest commit
RUN git config --global advice.detachedHead false && \
    git clone https://github.com/SoftSec-KAIST/Smartian.git && \
    cd Smartian && \
    git checkout badd4ff && \
    (git submodule update --init --recursive || true) && \
    make

CMD ["/bin/bash"]