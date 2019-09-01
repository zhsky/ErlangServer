%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 19:50:47
%% @Doc:	DESC
%% @Last:	2019-09-01 19:51:41
%% ====================================================================

-module(game_app_sup).
-behaviour(supervisor).

-define(CHILD_SPEC(Mod),{Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
-define(SUP_FLAGS,{one_for_one,1,60}).
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		start_link/0
		,init/1
		,start_child/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
	StartList = [],
	{ok,{?SUP_FLAGS, StartList}}.

start_child(Mod) ->
	supervisor:start_child(?MODULE, ?CHILD_SPEC(Mod)).