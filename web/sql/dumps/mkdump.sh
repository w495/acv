# /usr/bin/env bash

## 
##  \file Утилита для создания дампа базы
##

##
##  В качестве аргумента можно указать имя базы данных.
##      ./mkdump my_db_name
##
##

default_db_name=fk

##
## main() ->
##
    db_name=$default_db_name
    if [ -z "$1" ]
    then
        db_name=$default_db_name
    else
        db_name=$1
    fi
    echo "Используем $db_name в качестве базы данных."
    pg_dump $db_name --no-privileges --no-owner --no-reconnect \
        > dump-$db_name-`date "+%Y-%m-%d_%H-%M-%S-%N"`.sql
