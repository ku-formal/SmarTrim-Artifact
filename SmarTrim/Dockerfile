FROM ubuntu:jammy

ARG JOBS=24

RUN apt-get update && apt-get --yes upgrade && \
    apt-get install --yes \
    software-properties-common curl \
    # required for `opam install z3`
    autoconf libgmp-dev pkg-config \
    # ocaml
    opam \
    # python
    python3.11 python3-pip
    
SHELL ["/bin/bash", "-c"]

RUN curl -L https://foundry.paradigm.xyz | bash && source /root/.bashrc && \
    /root/.foundry/bin/foundryup
    
ENV PATH="/root/.foundry/bin:$PATH"

# OCaml 5.1.1
RUN opam init --disable-sandboxing --yes && eval $(opam env) && \ 
    opam switch create 5.1.1 && opam switch set 5.1.1 && eval $(opam env)

# OCaml deps
RUN opam install --yes ocamlfind ocamlbuild dune && \
    opam install --yes z3 -j ${JOBS}
RUN mkdir /root/VeriSmart
COPY dune* /root/VeriSmart
COPY verismart.opam /root/VeriSmart
WORKDIR /root/VeriSmart
RUN opam install . --yes --deps-only

COPY ./src /root/VeriSmart/src

RUN eval $(opam env) && dune build && cp main.exe .. && rm -rf * && mv ../main.exe . && mkdir output

ENTRYPOINT ["/bin/bash"]

# build-command : docker build -t verismart:250405 .