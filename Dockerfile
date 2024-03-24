FROM ocaml/opam:debian-11-ocaml-5.1

ENV DEBIAN_FRONTEND=noninteractive

USER root
RUN apt-get update && apt-get install -y \
    pkg-config libpq-dev libgmp-dev libssl-dev libpcre3-dev \
    libpng-dev libjpeg62-turbo-dev \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/waq
RUN opam-2.1 update && opam-2.1 install alcotest-lwt
COPY --chown=opam waq.opam .
RUN opam-2.1 install . --deps-only
COPY --chown=opam . .
RUN eval $(opam-2.1 env) && opam-2.1 install . --deps-only && dune build

FROM debian:11-slim

ARG INSTALL_TMOLE
RUN apt-get update && apt-get install -y \
    libpq5 libgmp10 netbase ca-certificates imagemagick \
    libjpeg62-turbo libpng16-16 \
    && rm -rf /var/lib/apt/lists/*

RUN groupadd -r waq && useradd -r -g waq waq
USER waq:waq

WORKDIR /waq/
COPY --from=0 /home/opam/waq/_build/default/bin/main.exe ./waq
COPY --from=0 /home/opam/waq/static /static
