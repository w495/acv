%%% @file dao_acv_video.erl
%%%
%%%    Администрирование роликов.
%%%

-module(dao_acv_video).
-compile(export_all).


%%% Поля acv_video:
%%%
%%%         id
%%%         name
%%%         datestart
%%%         datestop
%%%         url
%%%         ref
%%%         wish
%%%         postroll
%%%         preroll
%%%         midroll
%%%         pauseroll
%%%         user_male
%%%         age_from
%%%         age_to

get_acv_videos(_) ->
    Query =
        "select "
            "acv_video.id, acv_video.name,"
            "acv_video.datestart, acv_video.datestop"
        "from acv_video;",
    dao:simple(Query).

get_acv_video(Acv_video_id) ->
    Query =
        "select "
            "acv_video.id, acv_video.name,"
            "acv_video.datestart, acv_video.datestop"
         "from acv_video "
        "where id = $1;",
    dao:simple(Query, [Acv_video_id]).

update_acv_video(_Arg) ->
    ok.

delete_acv_video({Acv_video_id, User_id}) ->
    ok.