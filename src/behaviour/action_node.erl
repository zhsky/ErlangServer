%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-17 12:33:44
%% @Doc:	DESC
%% @Last:	2019-09-18 11:08:25
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

execute(Action, InputParam) ->
	do_execute(Action,InputParam).

%% ===================================test node action===================================
do_execute(node1_action1,_InputParam) ->
	?INFO("node1_action1~n");
do_execute(node1_action2,_InputParam) ->
	?INFO("node1_action2~n");
do_execute(node2_action1,_InputParam) ->
	?INFO("node2_action1~n");
do_execute(node3_action1,_InputParam) ->
	?INFO("node3_action1~n");
do_execute(node3_action2,_InputParam) ->
	?INFO("node3_action2~n");

do_execute(Action,_InputParam) ->
	?WARNING("not find ~p~n",[Action]),
	ok.