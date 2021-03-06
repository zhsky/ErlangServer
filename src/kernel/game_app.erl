%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 19:41:10
%% @Doc:	DESC
%% @Last:	2019-09-18 09:21:05
%% ====================================================================

-module(game_app).
-include("log.hrl").
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
	error_logger:add_report_handler(logger_handle),

	{ok, SuperPid} = game_app_sup:start_link(),

	game_server:start(),
	{ok, SuperPid}.

pre_stop(_State) ->
	io:format("pre_stop~n"),
	ok.

stop(_State) ->
	io:format("stop~n"),
	ok.