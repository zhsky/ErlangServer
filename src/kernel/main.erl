%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 14:13:33
%% @Doc:	DESC
%% @Last:	2019-09-04 22:19:30
%% ====================================================================

-module(main).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start/0, 
	stop/0
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================
start() ->
	try
		io:format("main start~n"),
		application:ensure_all_started(sasl),
		application:ensure_all_started(ranch),
		application:ensure_all_started(server),
		ok
	catch
		_:Reason ->
			io:format("Reason ~w ~n", [Reason])
	end.

stop() ->
	try
		application:stop(server),
		application:stop(sasl),
		io:format("main stop~n"),
		init:stop()
	catch
		_:Reason ->	
			io:format("stop error reason ~p ~n", [Reason])
	end.