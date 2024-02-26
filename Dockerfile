FROM ocaml/opam:debian-11-ocaml-4.14
# NOTE: ocaml/opam:ubuntu-22.04-ocaml-4.14 can't fetch Dune 3.7. I don't know why.

ENV DEBIAN_FRONTEND=noninteractive

USER root
RUN apt-get update && apt-get install -y \
    pkg-config libpq-dev libgmp-dev libssl-dev libpcre3-dev libcurl4-gnutls-dev \
    libpng-dev libjpeg62-turbo-dev \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/waq
RUN opam update && opam install alcotest-lwt
COPY --chown=opam waq.opam .
RUN opam install . --deps-only
COPY --chown=opam . .
RUN eval $(opam env) && opam install . --deps-only && dune build

FROM debian:11-slim

ARG INSTALL_TMOLE
RUN apt-get update && apt-get install -y \
    libpq5 libgmp10 netbase ca-certificates imagemagick libcurl3-gnutls \
    libjpeg62-turbo libpng16-16 curl \
    && rm -rf /var/lib/apt/lists/*

RUN groupadd -r waq && useradd -r -g waq waq
USER waq:waq

WORKDIR /waq/
COPY --from=0 /home/opam/waq/_build/default/bin/main.exe ./waq
COPY --from=0 /home/opam/waq/static /static

RUN if [ -n "$INSTALL_TMOLE" ]; then curl -s https://tunnelmole.com/sh/install-linux.sh | bash; fi

CMD ["bash", "-c", "/waq/waq db:migrate && /waq/waq"]

# docker build . -t waq && docker run -it -v $PWD/config:/waq/config waq [/bin/bash]
