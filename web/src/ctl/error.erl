%%
%% Модуль для ошибок
%%

-module(error).
-compile(export_all).
 
-include("web.hrl").
-include("../include/web_session.hrl").

 

% Вернуть JSON с текстом ошибки ErrorMessage



return_json(Request, ErrorMessage) ->
    JsonStruct = {struct, [{<<"ERROR">>, list_to_binary(ErrorMessage)}]},
    DataOut = mochijson2:encode(JsonStruct),
    Request:ok({?OUTPUT_JSON, [], [DataOut]}).

% Вернуть HTML с текстом ошибки ErrorMessage
return_html(Request, ErrorMessage) ->
    XslPath = "xsl/normal/outside/error.xsl",
    Xml  = xml:encode_data(
        [
            {
				"meta",  [ 	{"errormessage",ErrorMessage} ]
			}            
        ]
    ), 
    ResultHtml = xslt:apply(XslPath, Xml), 
    Request:ok({?OUTPUT_HTML, [], [ResultHtml]}).

redirect(Request, URI) -> 
	Val = authorization:get_customer_id(Request),
	Request:respond({302, [{"Location", URI}, {"Content-Type", ?OUTPUT_HTML}] ++ [mochiweb_cookies:cookie(?AUTHCOOKIE, Val, ?F_COOKIEOPTIONS)],""}).