%%% @file dao_customer.erl
%%%
%%%     Доступ к данным для сущности пользователя.
%%%     Описаны опирации с пользователями, группами и правами.
%%%     Версия для админки
%%%

-module(dao_customer).

-include("web_session.hrl").
-include("common.hrl").

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
	update_customer_profile/1,
    update_customer/1,
    delete_customer/1,
    test/0,
    test/1
]).

get_permissions(_) ->
    Query =
        "select "
            " p.id, p.name, p.description, "
            " p.type, p.perm_type_id, p.entity_id "
        " from permission as p;",
    dao:simple(Query).

% ============================================================================
% % CUSTOMER_GROUPS
% ============================================================================

get_customer_groups(_) ->
    Query =
        "select "
            "id, name, description "
        "from "
            "customer_group "
        "where "
            "customer_group.deleted = false;",
    dao:simple(Query).

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


%%%
%%% @doc
%%%     Удаляет группу пользователей если
%%%         она не является системной (c правом undel_group),
%%%         и если он сам не входит в эту группу.
%%%
delete_customer_group({Id, Updater_id}) ->
    Query =
        " update "
            " customer_group "
        " set "
            " deleted = true "
        " where "
            " id not in "
                " ( "
                    " ( "
                        % нельзя удалять группы c правом undel_group
                        "select "
                            " group_id "
                        " from "
                            " permission2group "
                        " join permission on "
                            " permission.id = permission2group.perm_id "
                            " and permission.name='undel_group' "
                    " ) "
                " union "
                    " ( "
                        % нельзя удалять группы в которые входишь сам
                        " select "
                            " group_id "
                        " from "
                            " customer2group "
                        " where "
                            " customer2group.customer_id = $2 "
                    " ) "
                " ) "
            " and id = $1; ",
    case dao:simple_ret(Query, [Id, Updater_id]) of
        {ok, 0} ->
            flog:error("Try to delete system or self customer_group !!!"),
            {warn, {"delete system or self group", []}};
        {ok, 1} ->
            ok;
        Error ->
            ?E("Error = ~p", [Error])
    end.


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
    Q = "select "
            "customer.id, customer.firstname, customer.lastname, "
            "customer.patronimic, customer.login, customer.pic_url, "
            "customer.active, "
            "customer.password_hash "
         "from "
            "customer "
         "where "
            "customer.deleted = false;",
    dao:simple(Q).

%-
get_customer(Id) ->
    Q1 = "select customer.id, "
                "customer.firstname, customer.lastname, customer.patronimic, "
                "customer.city, customer.organization, customer.position, "
                "customer.email, customer.login, customer.pic_url, customer.password_hash, customer.telepnone_number "
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

update_customer({{Firstname, Lastname, Patronimic, Login, Telephone, Email, City, Organization, Position}, Password_hash, GroupList, _updater_id}) ->
    update_customer({{null, Firstname, Lastname, Patronimic, Login, Telephone, Email, City, Organization, Position}, Password_hash, GroupList, _updater_id});

update_customer({{null, Firstname, Lastname, Patronimic, Login, Telephone, Email, City, Organization, Position}, Password_hash, GroupList, _updater_id}) ->
    update_customer({{null, Firstname, Lastname, Patronimic, Login, Telephone, [], Email, City, Organization, Position}, Password_hash, GroupList, _updater_id});

update_customer({{null, Firstname, Lastname, Patronimic, Login, Telephone, Pic_url, Email, City, Organization, Position}, Password_hash, GroupList, _updater_id}) ->
    Q1 = "insert into customer ("
			"firstname, "
			"lastname, " 
			"patronimic, "
            "login, "
			"telephone_number, "
			"pic_url, "
			"email, "
			"city, "
			"organization, "
			"position, "
			"password_hash) "
         "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11) returning customer.id;",
         
    PGRet = dao:with_transaction_fk(
        fun(Con) ->
            {ok, 1, _, [{Id}]} = pgsql:equery(Con, Q1,
                [Firstname, Lastname, Patronimic, Login, Telephone, Pic_url, Email, City, Organization, Position, Password_hash]),
            case length(GroupList) of
                0 ->
                    ok;
                L ->
                    Q2 = "insert into customer2group (customer_id, group_id) values " ++
                        string:join([lists:flatten(io_lib:format("(~p, ~p)",
                            [Id, X])) || X <- GroupList], ", "),
                    {ok, L} = pgsql:equery(Con, Q2, [])
            end,
            {return, Id}
        end
    ),
    dao:pgret(PGRet);

%%
%% Изменяет существующего пользователя
%%
update_customer({{Id, Firstname, Lastname, Patronimic, Login, Telephone, Pic_url, Email, City,
                    Organization, Position}, Password_hash, GroupList, _updater_id}) ->

    Q1 = "update customer set "
			"firstname = $1, "
			"lastname = $2, "
			"patronimic = $3, "
            "login = $4, "
			"telephone_number = $5, "
			"pic_url = $6, "
			"email = $7, "
            "city = $8, "
			"organization = $9, "
			"position = $10 "
         "where id=$11;",

    Q2 = "delete from customer2group where customer_id = $1;",
    Q3 = "insert into customer2group (customer_id, group_id) values " ++ 
            string:join([lists:flatten(io_lib:format("(~p, ~p)",
                [Id, X])) || X <- GroupList], ", "),

    PGRet = dao:with_transaction_fk(
        fun(Con) ->
             {ok, 1} = pgsql:equery(Con, Q1,
                    [Firstname, Lastname, Patronimic, Login, Telephone, Pic_url,
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
            end
            %,{return, Id}
        end
    ),
    dao:pgret(PGRet).

update_customer_profile({{ Firstname, Lastname, Patronimic, Pic_url, Email, City, Organization, Position, Telephone}, Password_hash, _updater_id}) ->

    Q1 = "update customer set firstname = $1, lastname = $2, patronimic = $3, "
            "pic_url = $4, email = $5,"
            "city = $6, organization = $7, position = $8, telephone_number = $9 "
         "where id=$10;",

    PGRet = dao:with_transaction_fk(
        fun(Con) ->
             {ok, 1} = pgsql:equery(Con, Q1,
                    [Firstname, Lastname, Patronimic, Pic_url,
                        Email, City, Organization, Position, Telephone, _updater_id]),
             if Password_hash =/= null ->
                    {ok, 1} = pgsql:equery(Con, "update customer set password_hash=$1 "
                        "where id = $2;", [Password_hash, _updater_id]);
                true ->
                    ok
             end 
        end
    ),
    dao:pgret(PGRet).

%%%
%%% @doc
%%%     Удаляет пользователя если
%%%         он не является системным,
%%%         и если он не пытается удалить сам себя
%%%     + Удаляет все рекламные кампании пользователя
%%%
delete_customer({Id, Updater_id}) ->
    Customer_query =
        "update customer set deleted=true "
            " where id = $1 "
                " and id not in ( "
                    " select "
                        " customer2group.customer_id "
                    " from "
                        " customer2group "
                    " join permission2group on "
                        " customer2group.group_id = permission2group.group_id "
                    " join permission on "
                            " permission.id = permission2group.perm_id "
                        " and "
                            " permission.name='undel_customer' "
                    " union "
                        " select $2 "
                "); ",

    Acv_video_Query =
        "update acv_video set deleted = true "
            "where customer_id=$1;",

    case dao:simple_ret(Customer_query, [convert:to_integer(Id), convert:to_integer(Updater_id)]) of
        {ok, 1} ->
            dao:simple(Acv_video_Query, [convert:to_integer(Id)]);
        {ok, 0} ->
            flog:error("Try to delete system user or self!!!"),
            {warn, {"delete system user or self", []}};
        Error ->
            ?E("Error = ~p", [Error])
    end.

test()->


    ok.

test(speed)->
    ok.
