#!/usr/bin/bash -e

dune exec bin/main.exe db:reset >/dev/null 2>/dev/null
dune exec bin/main.exe >/dev/null 2>/dev/null
