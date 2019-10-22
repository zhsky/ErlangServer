%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-10-22 11:06:34
%% @Doc:	DESC
%% @Last:	2019-10-22 12:53:01
%% ====================================================================

-module(http_handle).
-include("log.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		do/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

do(Req) ->
	Cmd = cowboy_req:binding(cmd,Req),
	Qs = cowboy_req:parse_qs(Req),
	{ok,Term} = handle_qs(Cmd,Qs),
	{ok,util:term_to_binary(util:term_to_string(Term))}.

%%http://127.0.0.1:8081/sky/test?sign=d1d664473260701bed08619ceafbed36&time=1571719145
handle_qs(<<"test">>,Qs) ->
	Sign = lists:keyfind(<<"sign">>,1,Qs),
	?INFO("test sign:~p",[Sign]),
	Respond = [1,[2,3],<<"4,5,6">>],
	{ok,Respond};

handle_qs(Cmd,Qs) ->
    ?WARNING("handle cmd:~s,QueryString:~s fail~n",[Cmd,Qs]),
    throw(<<"inval path">>).
