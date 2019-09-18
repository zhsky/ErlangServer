%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-17 12:36:12
%% @Doc:	DESC
%% @Last:	2019-09-18 10:56:11
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

is_true(#node_cond{cond_type = CondType,cond_list = CondList},InputParam) ->
	do_is_true(CondType,CondList,InputParam).

do_is_true(_,undefined,_InputParam) -> true;

do_is_true(?DEFAULT,[Cond],InputParam) -> 
	do_is_true2(Cond,InputParam);
do_is_true(?DEFAULT,Cond,InputParam) -> 
	do_is_true2(Cond,InputParam);
do_is_true(?OR,[],_InputParam) -> false;
do_is_true(?OR,[Cond | CondList],InputParam) ->
	case do_is_true2(Cond,InputParam) of
		true -> true;
		_ -> do_is_true(?OR,CondList,InputParam)
	end;
do_is_true(?AND,[],_InputParam) -> true;
do_is_true(?AND,[Cond | CondList],InputParam) ->
	case do_is_true2(Cond,InputParam) of
		true -> do_is_true(?AND,CondList,InputParam);
		_ -> false
	end;
do_is_true(CondType, Cond, _3) ->
	?WARNING("NOT HERE ~p,~p~n",[CondType,Cond]),
	false.




%% ===================================test node cond===================================
do_is_true2(node1_cond,InputParam) ->
	Flag = InputParam:value() > 100,
	?INFO("node1_cond:~p~n",[Flag]),
	Flag;

do_is_true2(node2_cond,InputParam) ->
	Flag = InputParam:value() > 90,
	?INFO("node2_cond:~p~n",[Flag]),
	Flag;
do_is_true2(node2_cond1,InputParam) ->
	Flag = InputParam:value() > 92,
	?INFO("node2_cond1:~p~n",[Flag]),
	Flag;
do_is_true2(node2_cond2,InputParam) ->
	Flag = InputParam:value() > 999,
	?INFO("node2_cond2:~p~n",[Flag]),
	Flag;
do_is_true2(node2_cond3,InputParam) ->
	Flag = InputParam:value() > 96,
	?INFO("node2_cond3:~p~n",[Flag]),
	Flag;

do_is_true2(node3_cond1,InputParam) ->
	Flag = InputParam:value() > 999,
	?INFO("node3_cond1:~p~n",[Flag]),
	Flag;
do_is_true2(node3_cond2,InputParam) ->
	Flag = InputParam:value() > 70,
	?INFO("node3_cond2:~p~n",[Flag]),
	Flag;
do_is_true2(node3_cond3,InputParam) ->
	Flag = InputParam:value() > 60,
	?INFO("node3_cond3:~p~n",[Flag]),
	Flag;
do_is_true2(node3_cond4,InputParam) ->
	Flag = InputParam:value() > 50,
	?INFO("node3_cond4:~p~n",[Flag]),
	Flag;

do_is_true2(Cond,_InputParam) ->
	?WARNING("not find ~p~n",[Cond]),
	false.