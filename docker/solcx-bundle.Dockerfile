FROM python:3.9-slim
#ENV SOLCX_BINARY_PATH="/root/.solcx/"
RUN pip install --upgrade pip && python3 -m pip install py-solc-x
RUN mkdir -p .solcx
COPY assets/install-solc.py /root
ENTRYPOINT ["/bin/bash"]

# install solc by solcx
# run:
# docker volume create solcx-bundle &&
# docker run --rm --volume solcx-bundle:/root/.solcx --entrypoint python3 solcx-bundle:latest /root/install-solc.py