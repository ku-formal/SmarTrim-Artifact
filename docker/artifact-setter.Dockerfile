FROM ubuntu:jammy 

RUN apt-get update && \
    apt-get install --yes python3-dev python3-pip docker.io
    
RUN apt-get install python-is-python3
RUN pip install numpy pandas matplotlib solc-select

COPY ./scripts/*.py /root/smartrim-experiment/scripts/
COPY ./docker/*.sh /root/sh-utils/

ENTRYPOINT [ "/bin/bash" ]