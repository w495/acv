%%% @file dao_customer.erl
%%%
%%%     Доступ к данным для сущности пользователя.
%%%     Описаны опирации с пользователями, группами и правами.
%%%     Версия для админки
%%%

-module(dao_customer).

-export([
% PERMISSIONS
    get_permissions/1,
% CUSTOMER_GROUPS
    get_customer_groups/1,
    get_customer_group/1,
    update_customer_group/1,
    delete_customer_group/1,
% CUSTOMERS
    get_customers/1,
    get_customer/1,
    get_customer_by_login/1,
    update_customer/1,
    delete_customer/1,
    test/0,
    test/1
]).

get_permissions(_) ->
    Q = "select p.id, p.name, p.description, p.type, p.perm_type_id, p.entity_id from permission as p;",
    dao:simple(Q).

% ============================================================================
% % CUSTOMER_GROUPS
% ============================================================================

get_customer_groups(_) ->
    Q = "select id, name, description from customer_group WHERE customer_group.deleted = false;",
    dao:simple(Q).

get_customer_group(Id) ->
    Q1 = "select id, name, description from customer_group where customer_group.id = $1;",
    io:format("Q1 = ~p~n", [Q1]),
    case dao:simple(Q1, [convert:to_integer(Id)]) of
        {ok, R1Val} ->
            Q2 = "select perm_id from permission2group where group_id = $1;",
            io:format("Q2 = ~p~n", [Q2]),
            case dao:simple(Q2, [convert:to_integer(Id)]) of
                {ok, R2Val} -> {ok, R1Val, [X || [{"perm_id", X}] <- R2Val]};
                E2 -> E2
            end;
        E1 -> E1
    end.

delete_customer_group({Id, _updater_id}) ->
    Q = "update customer_group set deleted=true "
        "where "
            " issystem = false " % нельзя удалять системные группы.
            " and id = $1; ",
    dao:simple(Q, [Id]).

update_customer_group({{null, Name, Descr}, PermissionList, _updater_id}) ->
    Q1 = "insert into customer_group (name, description) values ($1, $2) returning customer_group.id;",
    Ret = dao:with_transaction_fk(fun(Con) ->
        {ok, 1, _, [{Id}]} = pgsql:equery(Con, Q1, [Name, Descr]) ,
        io:format("New _customer_groupID: ~p~n", [Id]) ,
        case length(PermissionList) of
            0 -> ok;
            L ->
                Q2 = lists:append(["insert into permission2group (group_id, perm_id) values ",
                                    string:join([lists:flatten(io_lib:format("(~p, ~p)", [Id, X])) || X <- PermissionList], ", ")]),
                {ok, L} = pgsql:equery(Con, Q2, []),
                ok
        end
    end),
    dao:pgret(Ret);

update_customer_group({{Id, Name, Descr}, PermissionList, _updater_id}) ->
    Q1 = "update customer_group set name = $1, description = $2 where id = $3;",
    Q2 = "delete from permission2group where group_id = $1;",
    Q3 = "insert into permission2group (group_id, perm_id) values " ++ 
            string:join([lists:flatten(io_lib:format("(~p, ~p)", [Id, X])) || X <- PermissionList], ", "),
    Ret = dao:with_transaction_fk(
        fun(Con) ->
             {ok, 1} = pgsql:equery(Con, Q1, [Name, Descr, Id]),
             {ok, _} = pgsql:equery(Con, Q2, [Id]),
             case length(PermissionList) of
                0 -> ok;
                L -> {ok, L} = pgsql:equery(Con, Q3, []), ok
            end
        end
    ),
    dao:pgret(Ret).

% ============================================================================
% % CUSTOMERS
% ============================================================================

%+
get_customers(_) ->
    Q = "select customer.id, customer.firstname, customer.lastname, customer.patronimic, customer.login, customer.pic_url, "
                "customer.password_hash "
         "from customer WHERE customer.deleted = false;",
    dao:simple(Q).

%-
get_customer(Id) ->
    Q1 = "select customer.id, "
                "customer.firstname, customer.lastname, customer.patronimic, "
                "customer.city, customer.organization, customer.position, "
                "customer.email, customer.login, customer.pic_url, customer.password_hash "
            "from customer where customer.id=$1;",
    case dao:simple(Q1, [convert:to_integer(Id)]) of
        {ok, R1Val} ->
            Q2 = "select group_id from customer2group where customer_id = $1",
            case dao:simple(Q2, [convert:to_integer(Id)]) of
                {ok, R2Val} -> {ok, R1Val, [X || [{"group_id", X}] <- R2Val]};
                E2 -> E2
            end;
        E1 -> E1
    end.


%+
get_customer_by_login(Login) ->
    Q1 = "select customer.id, customer.firstname, customer.lastname, customer.patronimic, "
            "customer.login, customer.password_hash "
         "from customer where customer.login=$1 and customer.deleted=false;",
    case dao:simple(Q1, [Login]) of
        {ok, R1Val} ->
            Q2 =    "select permission.name "
                    "from customer   join customer2group on customer.id=customer2group.customer_id "
                                    "join permission2group on permission2group.group_id=customer2group.group_id "
                                    "join permission on permission.id=permission2group.perm_id "
                    "where customer.login=$1;",
            case dao:simple(Q2, [Login]) of
                {ok, R2Val} -> {ok, R1Val, [X || [{"name", X}] <- R2Val]};
                E2 -> E2
            end;
        E1 -> E1
    end.

%%
%% Создает нового пользователя
%%
update_customer({{null, Firstname, Lastname, Patronimic, Login, Pic_url, Email, City,
                    Organization, Position}, Password_hash, GroupList, _updater_id}) ->
    Q1 = "insert into customer (firstname, lastname, patronimic, "
            "login, pic_url, email, city, organization, position, password_hash) "
         "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10) returning customer.id;",
         
    PGRet = dao:with_transaction_fk(
        fun(Con) ->
            {ok, 1, _, [{Id}]} = pgsql:equery(Con, Q1,
                [Firstname, Lastname, Patronimic, Login, Pic_url, Email,
                    City, Organization, Position, Password_hash]),
            case length(GroupList) of
                0 ->
                    ok;
                L ->
                    Q2 = "insert into customer2group (customer_id, group_id) values " ++
                        string:join([lists:flatten(io_lib:format("(~p, ~p)",
                            [Id, X])) || X <- GroupList], ", "),
                    {ok, L} = pgsql:equery(Con, Q2, [])
            end,
            ok
        end
    ),
    dao:pgret(PGRet);

%%
%% Изменяет существующего пользователя
%%
update_customer({{Id, Firstname, Lastname, Patronimic, Login, Pic_url, Email, City,
                    Organization, Position}, Password_hash, GroupList, _updater_id}) ->

    Q1 = "update customer set firstname = $1, lastname = $2, patronimic = $3, "
            "login = $4, pic_url = $5, email = $6,"
            "city = $7, organization = $8, position = $9 "
         "where id=$10;",

    Q2 = "delete from customer2group where customer_id = $1;",
    Q3 = "insert into customer2group (customer_id, group_id) values " ++ 
            string:join([lists:flatten(io_lib:format("(~p, ~p)",
                [Id, X])) || X <- GroupList], ", "),

    PGRet = dao:with_transaction_fk(
        fun(Con) ->
             {ok, 1} = pgsql:equery(Con, Q1,
                    [Firstname, Lastname, Patronimic, Login, Pic_url,
                        Email, City, Organization, Position, Id]),
             if Password_hash =/= null ->
                    {ok, 1} = pgsql:equery(Con, "update customer set password_hash=$1 "
                        "where id = $2;", [Password_hash, Id]);
                true ->
                    ok
             end,
             {ok, _} = pgsql:equery(Con, Q2, [Id]),
             case length(GroupList) of
                0 -> ok;
                L -> {ok, L} = pgsql:equery(Con, Q3, [])
            end,
            ok
        end
    ),
    dao:pgret(PGRet).


delete_customer({Id, _updater_id}) ->
    Query = "update customer set deleted=true where id = $1;",
    dao:simple(Query, [convert:to_integer(Id)]).


test()->
    ok.

test(speed)->
    ok.
