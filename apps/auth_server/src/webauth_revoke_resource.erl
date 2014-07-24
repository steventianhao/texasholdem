-module(webauth_revoke_resource).
-export([init/1,content_types_provided/2,to_json/2,to_html/2]).

-include_lib("webmachine/include/webmachine.hrl"). 

init([])->
	{ok,undefined}.

content_types_provided(ReqData, Context) ->
   {[{"text/html", to_html},{"application/json",to_json},{"text/plain",to_text}], ReqData, Context}.

to_html(ReqData,State)->
	case revoke_token(ReqData) of
		{ok,_}->
			Reply="Deleted";
		{error,_}->
			Reply="Errors"
	end,
	Result=["<html><body>",Reply,"</body></html>"],
	{Result,ReqData, State}.

%application/json
to_json(ReqData,State)->
	case revoke_token(ReqData) of
		{ok,_}->
			Reply=[{<<"code">>,1}];
		{error,_}->
			Reply=[{<<"code">>,0}]
	end,
	Result=jsx:encode(Reply),
	{Result,ReqData,State}.

revoke_token(ReqData)->
	Token=wrq:path_info(token,ReqData),
	mcd:delete(cluster1,list_to_binary(Token)).