
-module(mysql_con_pool).

-define(NUMBER, 10).

-export([
    start_link/5,
    start_link/6,
    test/0,
    test/1
]).

start_link(PoolId, Host, User, Password, Database) ->
    start_link(PoolId, Host, User, Password, Database, ?NUMBER).

start_link(PoolId, Host, User, Password, Database, Number) ->
    Link = mysql:start_link(PoolId,Host,User,Password,Database),
    start_link_iter(PoolId,Host,User,Password,Database, Number),
    Link.

start_link_iter(_PoolId, _Host, _User, _Password, _Database, 0) -> ok;
start_link_iter(PoolId, Host, User, Password, Database, Step) ->
    mysql:connect(PoolId,Host,undefined,User,Password,Database,true),
    start_link_iter(PoolId, Host, User, Password, Database, Step-1).


    %mysql:connect(pool,"192.168.2.102",undefined,"cff","k9an612e","AVSrv",true),
    %mysql:connect(pool,"192.168.2.110",undefined,"repl","yhbvft","vk",true),



test()->
    ok.

test(speed)->
    ok.
