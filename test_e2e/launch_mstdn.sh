#!/usr/bin/bash -e

MSTDN_PATH="/home/anqou/ano/mastodon"

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

export RAILS_ENV=development
cd $MSTDN_PATH
#rails db:migrate:reset
rails db:setup >/dev/null
mstdn_token=$(echo '\
    app = Doorkeeper::Application.create!({name: "fakeapp", redirect_uri: "http://example.com", scopes: "read write follow", website: ""}); \
    token = Doorkeeper::AccessToken.create!(application_id: app, resource_owner_id: 1, scopes: "read write follow" ); \
    token.token' | \
    rails c --no-sandbox | egrep '^=>' | awk -F'"' '{print $2}' | sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,3})*)?[mGK]//g")
echo "$mstdn_token"
#foreman start >&2 2>/dev/null &
foreman start >/dev/null 2>/dev/null &
mstdn_pid=$!

cleanup() {
  killtree $mstdn_pid INT
}
for sig in INT QUIT HUP TERM; do
  trap "
    cleanup
    trap - $sig EXIT
    kill -s $sig "'"$$"' "$sig"
done
trap cleanup EXIT

wait
