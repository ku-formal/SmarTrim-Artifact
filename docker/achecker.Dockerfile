FROM ubuntu:jammy

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        software-properties-common \
        curl \
        git \
        build-essential \
        python3.10 \
        python3.10-dev \
        python3.10-venv \
        python3.10-distutils \
        ca-certificates && \
    curl -sS https://bootstrap.pypa.io/get-pip.py | python3.10 && \
    ln -s /usr/bin/python3.10 /usr/bin/python && \
    ln -s /usr/local/bin/pip /usr/bin/pip && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /home

RUN git clone https://github.com/DependableSystemsLab/AChecker.git && \
    cd /home/AChecker && \
    pip install --no-cache-dir -r requirements.txt

CMD ["/bin/bash"]
