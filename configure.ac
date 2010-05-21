AC_INIT([couch-locks], [0.1], [], [])

AC_ERLANG_NEED_ERLC

AC_SUBST(couchdbver)

AC_ARG_WITH(
  couchdb-version,
  AS_HELP_STRING([--with-couchdb-version=VERSION], [CouchDB version]),
  [couchdbver="$withval"]
)

AC_PREFIX_DEFAULT(/usr/local)

AC_CONFIG_FILES([
  Makefile
  src/Makefile
])

AC_OUTPUT
