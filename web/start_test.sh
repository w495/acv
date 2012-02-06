#!/bin/sh

    PATH=$PATH:/usr/local/lib/
    export PATH

    NODENAME="advsystem@localhost"
    CONFIG=app_dev

    ERL_ARGS="+K true +A 128 +P 1000000"

    ERL_MAX_ETS_TABLES=140000
    export ERL_MAX_ETS_TABLES

    echo "**************************************"
    LOCALLBINPATH=`find ./ebin -type d -printf '%h/%f '`
    OTHERBINPATH="./deps/*/ebin"
    BINPATH=${LOCALLBINPATH}${OTHERBINPATH}
    erl \
        -pa $BINPATH \
        -detached \
        -boot start_sasl \
        -config ${CONFIG} \
        -sname ${NODENAME} \
        -s reloader \
        -s web \
        -mnesia dir '"./priv/session-db"' \
        ${ERL_ARGS} \
    "$@"
