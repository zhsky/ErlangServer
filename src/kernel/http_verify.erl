%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-10-22 09:53:24
%% @Doc:	DESC
%% @Last:	2019-10-22 12:42:46
%% ====================================================================

-module(http_verify).
-include("log.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		init/2
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

init(Req, Opts) ->
	try
		verify_ip(cowboy_req:peer(Req)),
		verify_sign(Req),
		{ok,Respond} = http_handle:do(Req),
		cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Respond, Req)
	catch
		_:Err when is_binary(Err) ->
			?WARNING("~p",[Err]),
			cowboy_req:reply(400, #{}, Err, Req);
		_:Err:Trace ->
			?WARNING("~p,~p",[Err,Trace]),
			cowboy_req:reply(503, #{}, <<"503">>, Req)
	end,
	{ok, Req, Opts}.

verify_ip({Ip,Port}) ->
	?INFO("accept ~p:~p",[Ip,Port]),
	L = game_config:get(white_list),
	case lists:member(Ip,L) of
		true -> skip;
		_ -> throw(<<"inval IP">>)
	end.

verify_sign(Req) ->
	#{sign := Sign,time := _Time} = cowboy_req:match_qs([{sign, [], <<"undefined">>},{time, [], <<"0">>}], Req),
	Now = util:unixtime(),
	Time = list_to_integer(binary_to_list(_Time)),
	case abs(Now - Time) < 600000 of
		true -> skip;
		_ ->
			?WARNING("inval time:~p,server:~p",[Time,Now]),
			throw(<<"inval time">>)
	end,

	SignStr = binary:bin_to_list(Sign),
	case make_sign(Req) of
		SignStr -> skip;
		SerVerSign ->
			?WARNING("inval sign:~p,server:~p",[SignStr,SerVerSign]),
			throw(<<"inval sign">>)
	end.

make_sign(Req) ->
	_RsList = cowboy_req:parse_qs(Req),
	RsKvList = lists:sort(_RsList),
	Fun = fun
		({<<"sign">>,_},AccIn) -> AccIn;
		({_Key,Value},AccIn) ->
			<<AccIn/binary,Value/binary>>
	end,
	_RsBin = lists:foldl(Fun,<<>>,RsKvList),
	Key = game_config:get(http_key),
	RsBin = <<_RsBin/binary,Key/binary>>,

	RsStr = binary:bin_to_list(RsBin),
	SerVerSign = lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(RsStr))]),
	SerVerSign.