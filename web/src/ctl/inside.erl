%%% \file inside.erl
%%%
%%%     Контроллеры, активируются, когда пользователь уже зашел в систему
%%%

-module(inside).
 -compile(export_all).

-define(GAME_XML_PATH, "static/data/game/sets.xml").

-export([
%        call_before/1,
        call_after/1,
    % PERMISSIONS
        get_permissions/1,
    % CUSTOMER_GROUPS
        get_customer_groups/1,
        get_customer_group_info/1,
        update_customer_group/1,
        delete_customer_group/1,
    % CUSTOMERS
        get_customers/1,
        get_customer_info/1,
        update_customer/1,
        delete_customer/1]).
    % DIR


-import(mochiweb_cookies, [cookie/2]).
-include("../include/web_session.hrl").
-include("../include/common.hrl").

%-include_lib("xmerl/include/xmerl.hrl").

%call_before(Req) ->
%    authorization:auth_required(Req, "admin").
%    io:format("call_before(Req)"),
%    authorization:auth_required(Req).

call_after({Req, Result}) ->
    {Req, Result}.

% ============================================================================
% % PERMISSIONS
% ============================================================================

get_permissions(_Req) ->
    Res = dao:daoCall(dao_customer, getPermission, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % CUSTOMER_GROUPS
% ============================================================================

get_customer_groups(_Req) ->
    Res = dao:daoCall(dao_customer, getCustomerGroups, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_customer_group_info(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),

    io:format("Id = ~p~n", [Id]),

    case dao_customer:getCustomerGroup(Id) of
        {ok, Val, Perms} ->
            Res1 = db2json:encode(Val),
            Res2 = mochijson2:encode({struct, [{<<"value">>, Res1}, {<<"permissions">>, Perms}]});
        {error, E} -> Res2 = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res2]}.

update_customer_group(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    PermissionList = [utils:to_integer(X) || X <- proplists:get_all_values("permissions", Data)],

    E = norm:extr(Data, [{"id", [nullable, integer]}, 
                         {"name", [string]},
                         {"description", [string]}]),
    Res = dao:daoCall(dao_customer, updateCustomerGroup, {E, PermissionList, UID}),
    {"application/json", [], [mochijson2:encode(Res)]}.

delete_customer_group(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_customer, deleteCustomerGroup, {Id, UID}),
    {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % CUSTOMER
% ============================================================================

get_customers(_Req) ->
    Res = dao:daoCall(dao_customer, getCustomers, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_experts(_Req) ->
    Res = dao:daoCall(dao_customer, getExperts, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_customer_info(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]

    Id = utils:to_integer(proplists:get_value("id", Data)),
    case dao_customer:getCustomer(Id) of
        {ok, Val, Vals} ->
            Res1 = db2json:encode(Val),
            Res2 = mochijson2:encode({struct, [{<<"value">>, Res1}, {<<"groups">>, Vals}]});
        {error, E} ->
            Res2 = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res2]}.

update_customer(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    Pass = proplists:get_value("password", Data, ""),
    case Pass of
        "null" ->
            Pashash = null;
        _ when length(Pass) /= 0 ->
            Pashash = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(erlang:md5(Pass))]);
        _ ->
            Pashash = null
    end,
    E = norm:extr(Data, [{"id", [nullable, integer]},
                         {"firstname", [string]},
                         {"lastname", [string]},
                         {"patronimic", [string]},
                         {"login", [string]},
                         {"pic_url", [string]},
                         {"email", [nullable, string]},
                         {"city", [nullable, string]},
                         {"organization", [nullable, string]},
                         {"position", [nullable, string]}]),

    io:format("~p~n", [E]),
    GroupList = [utils:to_integer(X) || X <- proplists:get_all_values("groups", Data)],
    Res = dao:daoCall(dao_customer, updateCustomer, {E, Pashash, GroupList, UID}),
        {"application/json", [], [mochijson2:encode(Res)]}.

delete_customer(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),

    Data = Req:parse_post(),
    % % помни parse_post --- работает с POST
    % % Data = [{"key", "value"}, ... ]

    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_customer, deleteCustomer, {Id, UID}),
        {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % DIRS -> NEWS
% ============================================================================

get_news(Req) ->
    Res = dao:daoCall(dao_directory, getDocs, {news, doc}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_docs_recursive(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),

    Res = dao:daoCall(dao_directory, getDocsRecursive, {Id}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_docs(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),
    io:format("!!! utils:to_integer(proplists:get_value) "),
    Res = dao:daoCall(dao_directory, getDocs, {Id, doc}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_test_answers(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),
    io:format("!!! utils:to_integer(proplists:get_value) "),
    Res = dao:daoCall(dao_directory, getTestAnswers, {Id, doc}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.
    
get_doc_info(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),
    case dao_directory:getDoc(Id) of
        {ok, Val, Attaches} ->
            Val_ = db2json:encode(Val),
            Attaches_ = [db2json:encode([X]) || X <- Attaches ],
            Result = mochijson2:encode({struct,
                    [{<<"value">>, Val_ },
                     {<<"attaches">>, Attaches_}]
            });
        {error, E} ->
            Result = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Result]}.

get_test_answer_info(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),
    case dao_directory:getTestAnswer(Id) of
        {ok, Val, Attaches} ->
            Val_ = db2json:encode(Val),
            Attaches_ = [db2json:encode([X]) || X <- Attaches ],
            Result = mochijson2:encode({struct,
                    [{<<"value">>, Val_ },
                     {<<"attaches">>, Attaches_}]
            });
        {error, E} ->
            Result = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Result]}.

delete_doc(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_directory, deleteDocs, {Id, UID}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

delete_answer(Req) ->
    delete_doc(Req).

update_doc(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    % В кортеже описываются внутренние данные документа
    Info = norm:extr(Data, [{"id", [nullable, integer]},
                         {"name", [string]},
                         {"content", [string]},
                         {"published", [string]}
                        ]),
    Urls = norm:extr(Data, [{"pic_url", [nullable, string]}
                        ]),
    % В кортеже описываются внешние данные документа
    Placement = norm:extr(Data, [
                         {"dir_id", [integer]},
                         {"doc_type_id", [integer]}
                        ]),
    io:format("~n~p~n", [{Info, Urls, Placement}]),
    Res = dao:daoCall(dao_directory, updateDoc, {Info, Urls, Placement, UID}),
    io:format("3~n"),
    {"application/json", [], [mochijson2:encode(Res)]}.

update_answer(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    % В кортеже описываются внутренние данные документа
    Info = norm:extr(Data, [{"id", [nullable, integer]},
                         {"name", [string]},
                         {"content", [string]},
                         {"published", [string]},
                         {"correct_flag", [string]}
                        ]),
    Urls = norm:extr(Data, [{"pic_url", [nullable, string]}
                        ]),
    % В кортеже описываются внешние данные документа
    Placement = norm:extr(Data, [
                         {"dir_id", [integer]},
                         {"doc_type_id", [integer]}
                        ]),
    io:format("~n~p~n", [{Info, Urls, Placement}]),
    Res = dao:daoCall(dao_directory, updateTestAnswer, {Info, Urls, Placement, UID}),
    io:format("3~n"),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_attach_list(Req) ->
    Res = dao:daoCall(dao_customer, getPermission, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % DIRS -> ABSTR
% ============================================================================

get_dirs(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_directory, getDirs, {Id}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_conf_questions(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_directory, getConfQuestions, {Id}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


get_dirs_without(Req) ->
    Data = Req:parse_qs(),
    PId = utils:to_integer(proplists:get_value("id", Data)),
    CId = utils:to_integer(proplists:get_value("cid", Data)),
    Res = dao:daoCall(dao_directory, getDirs, {PId , CId}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.
    
    
get_dir_sons(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),
    %% io:format("Id  = ~p~n", [Id ]),
    Res = dao:daoCall(dao_directory, getDirSons, {Id}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_dir_info(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),
    % % io:format("Dir Id  = ~p~n", [Id]),
    case dao_directory:getDirA(Id) of
        {ok, Val, Attaches} ->
            Val_ = db2json:encode(Val),
            Attaches_ = [db2json:encode([X]) || X <- Attaches ],
            Result = mochijson2:encode({struct,
                    [{<<"value">>, Val_ },
                     {<<"attaches">>, Attaches_}]
            });
        {error, E} ->
            Result = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Result]}.
    
get_qdir_info(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),
    % % io:format("Dir Id  = ~p~n", [Id]),
    case dao_directory:getQDirA(Id) of
        {ok, Val, Attaches, Answ} ->
            Val_ = db2json:encode(Val),
            Answ_ = db2json:encode(Answ),
            Attaches_ = [db2json:encode([X]) || X <- Attaches ],
            Result = mochijson2:encode({struct,
                    [{<<"value">>, Val_ },
                     {<<"attaches">>, Attaches_},
                     {<<"count">>, Answ_}]
            });
        {error, E} ->
            Result = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Result]}.

get_conf_info(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    case dao_directory:getConfA(Id) of
        {ok, Val, Attaches, Experts} ->
            Val_ = db2json:encode(Val),
            Attaches_ = [db2json:encode([X]) || X <- Attaches ],
            Result = mochijson2:encode({struct,
                    [{<<"value">>, Val_ },
                     {<<"experts">>, Experts},
                     {<<"attaches">>, Attaches_}]
            });
        {error, E} ->
            Result = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Result]}.

%get_doc_info(Req) ->
%    Data = Req:parse_qs(),
%    % % помни parse_post --- работает с GET
%    % % Data = [{"key", "value"}, ... ]
%    Id = utils:to_integer(proplists:get_value("id", Data)),
%    case dao_directory:getDoc(Id) of
%        {ok, Val, Attaches} ->
%            Val_ = db2json:encode(Val),
%            Attaches_ = [db2json:encode([X]) || X <- Attaches ],
%            Result = mochijson2:encode({struct,
%                    [{<<"value">>, Val_ },
%                     {<<"attaches">>, Attaches_}]
%            });
%        {error, E} ->
%            Result = toBiz:mkErrorJson(E)
%    end,
%    {"application/json", [], [Result]}.

get_dir_parent_info(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]
    Id = utils:to_integer(proplists:get_value("id", Data)),
    case dao_directory:getDirMinInfo(Id) of
        {ok, Val} ->
            Res1 = db2json:encode(Val),
            Res2 = mochijson2:encode({struct, [{<<"value">>, Res1}]});
        {error, E} ->
            Res2 = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res2]}.

update_dir(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    % В кортеже описываются внутренние данные директории
    Info = norm:extr(Data, [{"id", [nullable, integer]},
                         {"name", [string]},
                         {"content", [nullable, string]}
                        ]),
                        
    Links = norm:extr(Data, [{"pic_url", [nullable, string]} ]),

    % В кортеже описываются внешние данные директории
    Placement = norm:extr(Data, [
                         {"parent_dir_id", [nullable, integer]},
                         {"dir_type_id", [nullable, integer]},
                         {"doc_description_id", [nullable, integer]}
            ]),
    Res = dao:daoCall(dao_directory, updateDir, {Info, Links, Placement, UID}),
        {"application/json", [], [mochijson2:encode(Res)]}.

% % % % % % % % % % % % % % % % % % % % % % % % % 
% Родителем директории становится 
%   текущая (открытая) директория
% % % % % % % % % % % % % % % % % % % % % % % % %

merge_dirs(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    % В кортеже описываются внутренние данные директории
    Info = norm:extr(Data, [
        {"cid", [integer]},
        {"id", [integer]}
    ]),
    % В кортеже описываются внешние данные директории

    Res = dao:daoCall(dao_directory, mergeDirs, Info),
        {"application/json", [], [mochijson2:encode(Res)]}.

update_conf(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    % В кортеже описываются внутренние данные директории
    Info = norm:extr(Data, [{"id", [nullable, integer]},
                         {"name", [string]},
                         {"content", [nullable, string]},
                         {"start", [nullable, integer]},
                         {"stop",  [nullable, integer]}
                        ]),
                        
    io:format("Info = ~p~n", [Info]),
    
    % В кортеже описываются внешние данные директории
    Placement = norm:extr(Data, [
                         {"parent_dir_id", [nullable, integer]},
                         {"dir_type_id", [nullable, integer]},
                         {"doc_description_id", [nullable, integer]}
            ]),
            
    ExpertList = [utils:to_integer(X) || X <- proplists:get_all_values("experts", Data)],
    
    
    io:format("ExpertList = ~p~n", [ExpertList]),
    
    Res = dao:daoCall(dao_directory, updateConf, {Info, Placement, UID, ExpertList}),
        {"application/json", [], [mochijson2:encode(Res)]}.

delete_dir(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_directory, deleteDir, {Id, UID}, values),
        {"application/json", [], [mochijson2:encode(Res)]}.

approve_conf_question(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    io:format("~n delete_conf_question ~n"),
    Data = Req:parse_post(),    
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_directory, approveConfQuestion, {Id, UID}, values),
        {"application/json", [], [mochijson2:encode(Res)]}.


delete_conf_question(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    io:format("~n delete_conf_question ~n"),
    Data = Req:parse_post(),    
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_directory, deleteConfQuestion, {Id, UID}, values),
        {"application/json", [], [mochijson2:encode(Res)]}.


update_game_map(Req) ->
    Data = Req:parse_post(),
    Info = norm:extr(Data, [
        {"id", [nullable, integer]},
        {"name", [string]},
        {"data", [string]}
    ]),
    Res = dao:daoCall(dao_game, updateMap, Info),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_game_maps(_Req) ->
   Res = dao:daoCall(dao_game, getMaps, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.


load_game_map(Req) ->
    Data = Req:parse_post(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_game, getMap, Id),
    {"application/json", [], [mochijson2:encode(Res)]}.


get_adv_coms(_Req) ->
    Res = dao:daoCall(dao_adv_com, getAdvComs, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_adv_com(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_adv_com, getAdvCom, Id),
    {"application/json", [], [mochijson2:encode(Res)]}.

update_adv_com(Req) ->
    Data = Req:parse_post(),
    Info = norm:extr(Data, [
        {"id", [nullable, integer]},
        {"name", [string]},
        {"ref", [string]},
        {"datestart", [datetimeUnixtime]},
        {"datestop", [datetimeUnixtime]},
        {"banner_place_id", [integer]},
        {"pic_url", [string]}
    ]),
    Res = dao:daoCall(dao_adv_com, updateAdvCom, Info),
    {"application/json", [], [mochijson2:encode(Res)]}.

delete_adv_com() ->
    test1:est().

get_banner_places(_Req) ->
    Res = dao:daoCall(dao_adv_com, getBannerPlaces, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_logs(_Req) ->
    Res = dao:daoCall(dao_bLog, getLogs, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_game_xml(_Req) ->
    case file:read_file(?GAME_XML_PATH) of
        {ok, Xml} -> Output = Xml;
        Error -> io:format("read game xml error: ~p~n", [Error]),
            Output = ""
    end,
    {"application/json", [], [mochijson2:encode({struct, [{<<"xml">>, Output}]})]}.


update_game_xml(Req) ->
    Data = Req:parse_post(),
    Xml = proplists:get_value("xml", Data),
    CC = filelib:ensure_dir(?GAME_XML_PATH),
    case file:write_file(?GAME_XML_PATH, Xml) of
        ok -> 
            Result = mochijson2:encode({struct, [{<<"result">>, ok}]});
        E -> 
            io:format("update_game_xml error: ~p~n", [E]),
            Result = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Result]}.


get_srcs(_Req) ->
    Res = dao:daoCall(dao_src, getSrcs, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_src_info(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_src, getSrc, Id),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_log_info(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    case dao_bLog:getLog(Id) of
        {ok, Val, Act} ->
            Val_ = db2json:encode(Val),
            Act_ = [db2json:encode([X]) || X <- Act ],
            Result = mochijson2:encode({struct,
                    [{<<"value">>, Val_ },
                     {<<"actions">>, Act_ }]
            });
        {error, E} ->
            Result = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Result]}.

update_src(Req) ->
    Data = Req:parse_post(),
    Info = norm:extr(Data, [
        {"id", [nullable, integer]},
        {"name", [string]},
        {"url", [string]},
        {"regexp", [string]},
        {"encoding", [string]}
    ]),
    Res = dao:daoCall(dao_src, updateSrc, Info),
    {"application/json", [], [mochijson2:encode(Res)]}.

delete_src(Req) ->
    Data = Req:parse_post(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_src, deleteSrc, Id),
    {"application/json", [], [mochijson2:encode(Res)]}.

run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    loop(Port,[], Timeout).

loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Port, Data++NewData, Timeout);
        {Port, {exit_status, 0}} -> Data;
        {Port, {exit_status, S}} -> throw({commandfailed, S})
    after Timeout ->
        throw(timeout)
end.

get_encoding(_Req) ->
    Res = dao:daoCall(dao_src, getEncoding, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.


get_adv_coms_vid(_Req) ->
    Res = dao:daoCall(dao_adv_com, getAdvComsVid, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_adv_com_vid(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:daoCall(dao_adv_com, getAdvComVid, Id),
    {"application/json", [], [mochijson2:encode(Res)]}.

update_adv_com_vid(Req) ->
    Data = Req:parse_post(),
    Info = norm:extr(Data, [
        {"id", [nullable, integer]},
        {"name", [string]},
        {"ref", [string]},
        {"datestart", [datetimeUnixtime]},
        {"datestop", [datetimeUnixtime]},
%        {"banner_place_id", [integer]},
        {"pic_url", [string]}
    ]),
    Res = dao:daoCall(dao_adv_com, updateAdvComVid, Info),
    {"application/json", [], [mochijson2:encode(Res)]}.


