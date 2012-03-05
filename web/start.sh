#!/bin/sh

NODENAME="advsystem@192.168.2.156"
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
    -name ${NODENAME} \
    -s web \
    -mnesia dir '"./priv/session-db"' \
    ${ERL_ARGS} \
    "$@"
