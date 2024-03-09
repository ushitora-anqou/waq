#!/usr/bin/bash -eu

set -o pipefail

NAME=$1
PORT=$2

PROG="
cd /home/node
npm i tunnelmole
cat <<EOS > tmole.js
const fs = require('node:fs');
const tunnelmole = require('tunnelmole/cjs');
(async () => {
  const url = await tunnelmole({ port: ${PORT} });
  const domain = new URL(url).hostname;
  console.log('OUTPUT:'+domain);
})()
EOS
node tmole.js"

docker run --init --rm --add-host=localhost:host-gateway --entrypoint 'bash' node -c "${PROG}" | grep --line-buffered OUTPUT | stdbuf -oL cut -d ':' -f2 | tee _test_${NAME}_domain
