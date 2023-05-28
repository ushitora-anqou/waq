FROM ocaml/opam:debian-11-ocaml-4.14
# NOTE: ocaml/opam:ubuntu-22.04-ocaml-4.14 can't fetch Dune 3.7. I don't know why.

ENV DEBIAN_FRONTEND=noninteractive

USER root
RUN apt-get update && apt-get install -y \
    pkg-config libpq-dev libgmp-dev libssl-dev libpcre3-dev libcurl4-gnutls-dev \
    libpng-dev libjpeg62-turbo-dev \
    && rm -rf /var/lib/apt/lists/*

USER opam
RUN opam update && opam install alcotest-lwt
COPY --chown=opam . waq/
WORKDIR /home/opam/waq
RUN opam install . --deps-only
RUN eval $(opam env) && dune build

FROM debian:11-slim

ARG INSTALL_TMOLE
RUN apt-get update && apt-get install -y \
    libpq5 libgmp10 netbase ca-certificates imagemagick libcurl3-gnutls \
    libjpeg62-turbo libpng16-16 curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /root/
COPY --from=0 /home/opam/waq/_build/default/bin/main.exe ./waq
COPY --from=0 /home/opam/waq/static /static

RUN if [ -n "$INSTALL_TMOLE" ]; then curl -s https://tunnelmole.com/sh/install-linux.sh | bash; fi

CMD ["bash", "-c", "/root/waq db:migrate && /root/waq"]

# docker build . -t waq && docker run -it -v $PWD/config:/root/config waq [/bin/bash]
