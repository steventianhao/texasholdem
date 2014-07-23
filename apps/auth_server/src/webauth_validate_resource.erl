-module(webauth_validate_resource).
-export([init/1,content_types_provided/2,to_json/2,to_html/2]).

-include_lib("webmachine/include/webmachine.hrl"). 

init([])->
	{ok,undefined}.

content_types_provided(ReqData, Context) ->
   {[{"text/html", to_html},{"application/json",to_json},{"text/plain",to_text}], ReqData, Context}.

to_html(ReqData,State)->
	case validate_token(ReqData) of
		{ok,OldToken}->
			Reply=OldToken;
		{error,_}->
			Reply="Errors"
	end,
	Result=["<html><body>",Reply,"</body></html>"],
	{Result,ReqData, State}.

%application/json
to_json(ReqData,State)->
	case validate_token(ReqData) of
		{ok,_OldToken}->
			Reply=[{<<"code">>,1}];
		{error,_}->
			Reply=[{<<"code">>,0}]
	end,
	Result=jsx:encode(Reply),
	{Result,ReqData,State}.

validate_token(ReqData)->
	Token=wrq:path_info(token,ReqData),
	mcd:get(cluster1,list_to_binary(Token)).