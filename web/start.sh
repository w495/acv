#!/bin/sh

NODENAME="advsystem@192.168.2.190"
CONFIG=app
COOKIE=OCFDAZSCILRVUEHEWFWA

ERL_ARGS="+K true +A 128 +P 1000000"

ERL_MAX_ETS_TABLES=140000
export ERL_MAX_ETS_TABLES

LOCALLBINPATH=`find ./ebin -type d -printf '%h/%f '`
OTHERBINPATH="./deps/*/ebin"
BINPATH=${LOCALLBINPATH}${OTHERBINPATH}

erl \
    -pa $BINPATH \
    -detached \
    -boot start_sasl \
    -config ${CONFIG} \
    -name ${NODENAME} \
    -s web \
    -mnesia dir '"./priv/session-db"' \
    ${ERL_ARGS} \
    "$@"
