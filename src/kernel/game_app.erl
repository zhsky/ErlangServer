%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 19:41:10
%% @Doc:	DESC
%% @Last:	2019-09-01 20:13:59
%% ====================================================================

-module(game_app).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start/2
	,stop/1
	,pre_stop/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

start(_Type,[]) ->
	io:format("game_app start~n"),
	game_config:init(),
	LogLevel = game_config:get(log_level),
	io:format("~p~n",[LogLevel]),
	{ok, SuperPid} = game_app_sup:start_link(),
	{ok, SuperPid}.

pre_stop(_State) ->
	io:format("pre_stop~n"),
	ok.

stop(_State) ->
	io:format("stop~n"),
	ok.