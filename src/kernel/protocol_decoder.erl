%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-03 21:25:08
%% @Doc:	DESC
%% @Last:	2019-09-03 21:48:57
%% ====================================================================

-module(protocol_decoder).
-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([
		start_link/0
	]).

-record(state,{}).
%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?MODULE, init, []).

init(_) ->
	{ok,#state{}}.

handle_info(_Info, State) ->
	{noreply,State}.

handle_call(_Info, _From, State) ->
	{noreply,State}.

terminate(_Info, #state{} = _State) ->
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

handle_cast(stop, State) ->
	{stop,normal,State};

handle_cast({decode,_Data}, State) ->
	%%%%%%%%%%%%%%%
	{noreply,State};

handle_cast(_Info, State) ->
	{noreply,State}.
