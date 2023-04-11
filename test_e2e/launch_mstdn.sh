#!/usr/bin/bash -e

[ -z "${MSTDN_PATH}" ] && exit 1

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
echo '\
$app = Doorkeeper::Application.create!({ name: "fakeapp", redirect_uri: "http://example.com", scopes: "read write follow", website: "" })

def f(create_new: true, username:)
  if create_new
    account = Account.new(username: username)
    password = "mastodon#{username}"
    email = "#{username}@example.com"
    user = User.new(email: email, password: password, agreement: true, approved: true, role_id: nil, confirmed_at: Time.now.utc, bypass_invite_request_check: true)

    account.suspended_at = nil
    user.account = account

    user.save!
    user.confirmed_at = nil
    user.confirm!
  end

  Doorkeeper::AccessToken.create!(
    application_id: $app,
    resource_owner_id: Account.find_by(username: username, domain: nil).user.id,
    scopes: "read write follow",
  ).token
end

puts("TOKEN: #{f(create_new: false, username: "admin")}")
puts("TOKEN: #{f(username: "mstdn1")}")
puts("TOKEN: #{f(username: "mstdn2")}")
puts("TOKEN: #{f(username: "mstdn3")}")
' | rails c --no-sandbox | egrep '^TOKEN:' | sed -r 's/^TOKEN: (.+)$/\1/'
if [ -z "$MSTDN_LOG" ]; then
  foreman start >/dev/null 2>/dev/null &
else
  foreman start >&2 2>/dev/null &
fi
mstdn_pid=$!

cleanup() {
  killtree $mstdn_pid KILL
}
for sig in INT QUIT HUP TERM; do
  trap "
    cleanup
    trap - $sig EXIT
    kill -s $sig "'"$$"' "$sig"
done
trap cleanup EXIT

wait
