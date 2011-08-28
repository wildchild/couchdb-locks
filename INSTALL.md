# Installation

It runs as a plugin, so you don't required to patch CouchDB.

## Compiling

    autoconf
    ./configure --prefix=/usr/local/couchdb --with-couchdb-version="0.11.0"
    make
    make install

## Configuration

Put the following lines to /etc/couchdb/local.d/locks.ini:

    [locks]
    timeout = 5000 ; milliseconds after which to mark lock as expired
    expire_interval = 3000 ; milliseconds after which to sweep expired locks

    [daemons]
    couch_locks = {couch_locks, start_link, []}

    [httpd_db_handlers]
    _locks = {couch_httpd_locks, handle_locks_req}

    [httpd_global_handlers]
    _all_locks = {couch_httpd_locks, handle_all_locks_req}

## Testing

Assuming that your couch is on http://localhost:5984. Otherwise change it in spec/spec\_helper.rb.

Install ruby and rubygems, then execute:

    gem install bundler
    bundle install --path vendor
    bundle exec rspec spec/

You'll see green lines.
