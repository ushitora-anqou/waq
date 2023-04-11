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

export WAQ_DEBUG_JOB_KICK_BLOCK=true
export WAQ_GENERATE_TEST_USERS=true
export WAQ_CONFIG_PATH="config/test.yml"
#export WAQ_DUMP_REQ_DIR="log/test_e2e"
dune exec bin/main.exe db:reset >/dev/null 2>/dev/null
dune exec bin/main.exe oauth:generate_access_token user1
dune exec bin/main.exe oauth:generate_access_token user2
dune exec bin/main.exe oauth:generate_access_token user3
dune exec bin/main.exe &
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
