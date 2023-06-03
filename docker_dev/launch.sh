#!/bin/bash -xeu

set -o pipefail

ARG_PROG=$1
echo "===== Launching $ARG_PROG ====="
echo "PWD = $PWD"

case "$ARG_PROG" in
    elk )
        LOCAL_PORT=5314
        ;;
    mastodon )
        LOCAL_PORT=3000
        ;;
    waq )
        LOCAL_PORT=8000
        ;;
esac

# Launch tunnelmole
SERVER_NAME_FILE_NAME=/data/$ARG_PROG/server_name
truncate -s 0 $SERVER_NAME_FILE_NAME
tmole $LOCAL_PORT | \
    grep --line-buffered "^https" | \
    sed -u -r 's/^https:\/\/([^ ]+) .*$/\1/' | \
    tee $SERVER_NAME_FILE_NAME &
while [ -z $(cat $SERVER_NAME_FILE_NAME) ]; do sleep 1; done
SERVER_NAME=$(head -1 $SERVER_NAME_FILE_NAME)
echo "SERVER_NAME = $SERVER_NAME"

# Launch specified program
case "$ARG_PROG" in
    elk )
        PORT=$LOCAL_PORT node build/server/index.mjs
        ;;

    mastodon )
        sed -i "87i   config.hosts << \"$SERVER_NAME\"" config/environments/development.rb
        cat <<EOS > .env
LOCAL_DOMAIN=$SERVER_NAME
LOCAL_HTTPS=true
DB_HOST=mastodon-postgres
DB_USER=postgres
DB_NAME=mastodon_development
DB_PASS=PASSWORD
DB_PORT=5432
REDIS_HOST=mastodon-redis
REDIS_PORT=6379
EOS
        cat <<EOS > Procfile.dev
web: env PORT=3000 RAILS_ENV=development BIND=0.0.0.0 bundle exec puma -C config/puma.rb
sidekiq: env PORT=3000 RAILS_ENV=development bundle exec sidekiq
stream: env PORT=4001 yarn run start
webpack: ./bin/webpack-dev-server --listen-host 0.0.0.0
EOS
        RAILS_ENV=development rails db:setup
        foreman start
        ;;

    waq )
        export SERVER_NAME=$SERVER_NAME
        export DB_URL="postgresql://postgres:PASSWORD@waq-postgres:5432/waq_dev"
        export VAPID_PRIVATE_KEY="5OS5_NrCP_jtMBQvYPuQyse8s3C-11YohjEeDAQR4oE"
        export VAPID_PUBLIC_KEY="BEsq0u2vxWcxoVye79FQX5HU4RYwgHYJXPX_YVb6A-tBqfm2JvaiFUVNofe6SGTsvWnKIEX9dOaxLVrktQ01bXc"
        export LOG_FILE_PATH=""
        /root/waq db:migrate
        /root/waq user:register --username admin --display-name Admin --email "admin@$SERVER_NAME" --password waqpassword
        /root/waq
        ;;
esac