#!/bin/sh

NODENAME="advsystem@localhost"
CONFIG=app
COOKIE=OCFDAZSCILRVUEHEWFWA

ERL_ARGS="+K true +A 128 +P 1000000"

ERL_MAX_ETS_TABLES=140000
export ERL_MAX_ETS_TABLES

LOCALLBINPATH=`find ./ebin -type d -printf '%h/%f '`
OTHERBINPATH="./deps/*/ebin"
BINPATH=${OTHERBINPATH}" "${LOCALLBINPATH}

erl \
    -pz $BINPATH \
    -detached \
    -boot start_sasl \
    -config ${CONFIG} \
    -sname ${NODENAME} \
    -s web \
    -mnesia dir '"./priv/session-db"' \
    ${ERL_ARGS} \
    "$@"
