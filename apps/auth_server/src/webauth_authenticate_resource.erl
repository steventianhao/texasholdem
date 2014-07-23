-module(webauth_authenticate_resource).
-export([init/1,content_types_provided/2,to_json/2,to_html/2]).

-include_lib("webmachine/include/webmachine.hrl"). 

init([])->
	{ok,undefined}.

content_types_provided(ReqData, Context) ->
   {[{"text/html", to_html},{"application/json",to_json},{"text/plain",to_text}], ReqData, Context}.

to_html(ReqData,State)->
	{_Token,Token2}=exchange_token(ReqData),
	{["<html><body>",Token2,"</body></html>"], ReqData, State}.

%application/json
to_json(ReqData,State)->
	{_Token,Token2}=exchange_token(ReqData),
	Result=jsx:encode([{<<"code">>,1},{<<"token">>,Token2}]),
	{Result,ReqData,State}.


exchange_token(ReqData)->
	Token=wrq:path_info(token,ReqData),
	Token2=uuid:to_string(uuid:uuid3(uuid:uuid4(), "simonvalorjacy")),
	mcd:async_set(cluster1,list_to_binary(Token2),list_to_binary(Token),24*60*60),
	{Token,Token2}.