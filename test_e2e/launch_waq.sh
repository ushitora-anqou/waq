#!/usr/bin/bash -e

# Thanks to: https://stackoverflow.com/a/3211182
# It is necessary to kill child processes recursively because node makes another process group.
killtree() {
    local _pid=$1
    local _sig=$2
    kill -STOP ${_pid}
    for _child in $(ps -o pid --no-headers --ppid ${_pid}); do
        killtree ${_child} ${_sig}
    done
    kill -${_sig} ${_pid}
}

dune exec bin/main.exe db:reset >/dev/null 2>/dev/null
dune exec bin/main.exe >/dev/null 2>/dev/null &
waq_pid=$!

cleanup() {
  killtree $waq_pid KILL
}
for sig in INT QUIT HUP TERM; do
  trap "
    cleanup
    trap - $sig EXIT
    kill -s $sig "'"$$"' "$sig"
done
trap cleanup EXIT

wait
