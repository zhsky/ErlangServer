%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-18 10:57:49
%% @Doc:	DESC
%% @Last:	2019-10-16 21:05:47
%% ====================================================================

-module(condition_node).
-include("behaviour.hrl").
-include("log.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
	is_true/2
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

is_true(#node_cond{cond_type = CondType,cond_list = CondList},Args) ->
	do_is_true(CondType,CondList,Args).

do_is_true(_,undefined,_Args) -> true;

do_is_true(?DEFAULT,[Cond],Args) -> 
	do_is_true2(Cond,Args);
do_is_true(?DEFAULT,Cond,Args) -> 
	do_is_true2(Cond,Args);
do_is_true(?OR,[],_Args) -> false;
do_is_true(?OR,[Cond | CondList],Args) ->
	case do_is_true2(Cond,Args) of
		true -> true;
		_ -> do_is_true(?OR,CondList,Args)
	end;
do_is_true(?AND,[],_Args) -> true;
do_is_true(?AND,[Cond | CondList],Args) ->
	case do_is_true2(Cond,Args) of
		true -> do_is_true(?AND,CondList,Args);
		_ -> false
	end;
do_is_true(CondType, Cond, _3) ->
	?WARNING("NOT HERE ~p,~p~n",[CondType,Cond]),
	false.

do_is_true2(CondFun,[Mod|Args] = _Args) ->
	try
		Mod:CondFun(Args)
	catch
		_:Err:Trace ->
			?ERROR("do_is_true2 ERROR: ~p:~p",[Err,Trace]),
			false
	end.