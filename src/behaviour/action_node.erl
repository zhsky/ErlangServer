%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-18 10:57:55
%% @Doc:	DESC
%% @Last:	2019-10-16 21:05:28
%% ====================================================================

-module(action_node).
-include("log.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute/2]).

%% ====================================================================
%% Internal functions
%% ====================================================================

execute(ActionFun, [Mod|Args]) ->
	try
		Mod:ActionFun(Args)
	catch
		_:Err:Trace ->
			?ERROR("execute ERROR ~p,~p",[Err,Trace]),
			false
	end.
