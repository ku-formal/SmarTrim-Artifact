FROM ubuntu:jammy

ARG DEBIAN_FRONTEND=noninteractive

# Space-separated version string without leading 'v' (e.g. "0.4.21 0.4.22") 
ARG SOLC

RUN apt-get update
RUN apt-get install -y \
     libsqlite3-0 \
     libsqlite3-dev \
     apt-utils \
     build-essential \
     locales \
     python3-pip-whl \
     python3-pip \
     python3-setuptools \
     software-properties-common && \
    add-apt-repository -y ppa:ethereum/ethereum && \
    apt-get update && \
    apt-get install -y \
     solc \
     libssl-dev \
     python3-dev \
     pandoc \
     git \
     wget \
  && ln -s /usr/bin/python3 /usr/local/bin/python
  
RUN mkdir /opt/lent-sse
  
RUN echo 'blake2b-py<0.2.2\n\
coloredlogs>=10.0\n\
coincurve>=13.0.0\n\
cython\n\
cytoolz<0.12.0\n\
asn1crypto>=0.22.0\n\
configparser>=3.5.0\n\
coverage>6.0\n\
py_ecc<5.0.0,>=1.4.7\n\
eth_abi<3.0.0,>=2.0.0b4\n\
eth-account<0.6.0,>=0.5.6\n\
ethereum-input-decoder>=0.2.2\n\
eth-hash<0.4.0,>=0.3.1\n\
eth-keyfile<0.6.0,>=0.5.1\n\
eth-keys<0.4.0,>=0.2.1\n\
eth-rlp<0.3.0,>=0.1.0\n\
eth-typing<3.0.0,>=2.1.0\n\
eth-utils<2\n\
hexbytes<0.3.0\n\
jinja2>=2.9\n\
MarkupSafe<2.1.0\n\
mock\n\
persistent>=4.2.0\n\
py-flags\n\
py-evm==0.5.0a1\n\
py-solc-x\n\
py-solc\n\
pytest>=3.6.0\n\
pyparsing<3,>=2.0.2\n\
pytest-cov\n\
pytest_mock\n\
requests\n\
rlp<3\n\
semantic_version\n\
transaction>=2.2.1\n\
typing-extensions<4,>=3.7.4\n\
z3-solver>=4.8.8.0\n\
matplotlib\n\
pre-commit\n\
certifi>=2020.06.20\n\
mypy-extensions<1.0.0\n\
exceptiongroup<1.3.0' > /opt/lent-sse/requirements.txt

WORKDIR /opt/lent-sse
RUN pip3 install -r requirements.txt

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.en
ENV LC_ALL=en_US.UTF-8

# ORIGINAL: COPY . /opt/mythril
# latest commit, 2025 AUG 1
WORKDIR /home
RUN git clone https://github.com/tczpl/lent-sse.git && \
    cd /home/lent-sse && \
    git reset --hard 322bdf6317fa4fdc52e0cdd5303d309fb918965a && \
    rm -fr dataset results_paper solidity_examples static top1000
# uncomment z3 timeout setting
WORKDIR /home/lent-sse
RUN sed -i '119c\    s.raw.set("timeout", timeout)' /home/lent-sse/mythril/support/model.py
RUN python setup.py install

RUN ( [ ! -z "${SOLC}" ] && set -e && for ver in $SOLC; do python -m solc.install v${ver}; done ) || true

ENTRYPOINT ["/usr/local/bin/myth"]
