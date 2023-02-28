FROM ocaml/opam:debian-11-ocaml-4.14
# NOTE: ocaml/opam:ubuntu-22.04-ocaml-4.14 can't fetch Dune 3.7. I don't know why.

ENV DEBIAN_FRONTEND=noninteractive

USER root
RUN apt-get update && apt-get install -y \
    pkg-config libpq-dev libgmp-dev libssl-dev \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam
RUN git clone https://github.com/ushitora-anqou/waq.git && \
    cd waq && \
    git checkout fed9f1e

WORKDIR /home/opam/waq
RUN opam update && opam install alcotest-lwt
RUN opam install . --deps-only
RUN eval $(opam env) && dune build

FROM debian:11-slim

RUN apt-get update && apt-get install -y \
    libpq5 libgmp10 netbase \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /root/
COPY --from=0 /home/opam/waq/_build/default/bin/main.exe ./waq

CMD ["/root/waq"]

# docker build . -t waq && docker run -it -v $PWD/config:/root/config waq [/bin/bash]
