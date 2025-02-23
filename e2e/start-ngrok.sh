#!/usr/bin/env bash

set -ux
set -o pipefail

NAME=$1
PORT=$2

[ -f .env ] && . .env
docker run --init --rm --net=host \
  -e NGROK_AUTHTOKEN=${NGROK_AUTHTOKEN} \
  -v $PWD/ngrok.yml:/home/ngrok/.config/ngrok/ngrok.yml \
  ngrok/ngrok:latest http ${PORT} --config /home/ngrok/.config/ngrok/ngrok.yml | \
  grep --line-buffered "started tunnel" | \
  sed -ru 's/^.* url=https:\/\/(.*)$/\1/g' | \
  tee _test_${NAME}_domain
