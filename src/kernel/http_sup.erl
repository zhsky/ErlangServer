%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-10-22 09:30:50
%% @Doc:	DESC
%% @Last:	2019-10-22 12:32:02
%% ====================================================================

-module(http_sup).
-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([
		start_link/0
		,upgrade/0
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
   gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init(_) ->
	Port = game_config:get(http_port),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/sky/:cmd", http_verify, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
		env => #{dispatch => Dispatch}
	}),
	{ok, []}.

upgrade() ->
	Pid = whereis(?MODULE),
	gen_server:cast(Pid,upgrade).

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(upgrade, State) ->
	ok = cowboy:stop_listener(http),
	init([]),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
