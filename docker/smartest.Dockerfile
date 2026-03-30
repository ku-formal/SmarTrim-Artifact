FROM ubuntu:jammy

# Install some basic pre-requisites
RUN apt-get update \
  && apt-get install --yes \
    sudo wget git \
    software-properties-common autoconf pkg-config libgmp3-dev \
    opam \
    time \
    python3 python3-pip \
    python2 python2-dev \
  && apt-get clean -q --yes \
  && rm -rf /var/lib/apt/lists/*

ARG VERISMART_URL=https://github.com/kupl/VeriSmart-public.git
ARG VERISMART_COMMIT=36d191eca5e82e52297ed78b2cf8ff2ce509f7d8

WORKDIR /build
RUN git clone $VERISMART_URL ./verismart

WORKDIR /build/verismart
RUN git config --global advice.detachedHead false \
  && git checkout $VERISMART_COMMIT

RUN opam init --yes --disable-sandboxing \
  && opam switch create 5.1.1 && opam switch set 5.1.1 \
  && eval $(opam env) \
  && opam update \
  && opam install --yes \
    conf-m4.1 ocamlfind ocamlbuild num yojson batteries ocamlgraph zarith \
  && opam install --yes z3.4.8.17
# Make sure that ocamlbuild and such exists in the path
RUN echo 'eval $(opam env)' >> $HOME/.bashrc

WORKDIR /build/verismart
RUN sed -i 's/Z3/z3/g' ./_tags && chmod +x build && eval $(opam env) && ./build && ./main.native --help >/dev/null
RUN ln -s $(realpath ./main.native) /usr/local/bin/verismart

CMD ["/bin/bash"]