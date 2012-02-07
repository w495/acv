%%% @file dao_cat.erl
%%%
%%%    Бизнес логика раздачи рекламных креативов.
%%%

-module(dao_cat).

-export([
    get_all_cats/1,
    get_clips_url/1,
    get_cat_by_seo_alias/1,
    test/0,
    test/1
]).

-include("common.hrl").

%%% @doc
%%% Возвращает список всех категорий
%%%     (список mark, c типом Categories)
%%%
get_all_cats(_) ->
    Query  = <<"select mark.id, mark.name, mark.seo_alias from mark "
                "join mark_type on mark.mark_type_id = mark_type.id "
            "where mark_type.name=\"Categories\"; ">>,
    mysql_dao:simple(Query).


get_cat_by_seo_alias(Seo_alias) ->
    Query  = <<"select mark.id, mark.name, mark.seo_alias from mark "
                "join mark_type on mark.mark_type_id = mark_type.id "
            "where mark_type.name=\"Categories\" and mark.seo_alias=?;">>,
    mysql_dao:simple(Query, [Seo_alias]).


get_clips_url(Id) ->

    Query =  <<"select clip.seo_alias, clip.url from clip join clip_mark on clip_mark.clip_id = clip.id "
            " join mark on clip_mark.mark_id = mark.id "
            " join mark_type on mark.mark_type_id = mark_type.id "
            " where mark_type.name=\"Categories\" and mark.id=?;">>,

    mysql_dao:simple(Query, [Id]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Юнит тестирование
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test()->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Нагрузочное тестирование
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(speed)->

    ok.

