%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-18 09:21:27
%% @Doc:	DESC
%% @Last:	2019-10-16 21:04:08
%% ====================================================================

-module(tree_test).
-behaviour(gen_server).
-include("behaviour.hrl").
-include("log.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start/0,enter_tree/0]).
-export([
		node1_cond/1
		,node2_cond/1
		,node2_cond1/1
		,node2_cond2/1
		,node2_cond3/1
		,node3_cond1/1
		,node3_cond2/1
		,node3_cond4/1
		,node1_action1/1
		,node1_action2/1
		,node2_action1/1
		,node3_action1/1
		,node3_action2/1
		,node3_cond3/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

init(_) ->
	Tree = tree_manager:get_tree(tree_10000),
	tree_main:enter_tree(#input_param{tree = Tree,args = [?MODULE]}),
	{ok,[]}.

handle_info(_Info, State) ->
	{noreply,State}.

handle_call(_Info, _From, State) ->
	{noreply,State}.

terminate(_Info, _State) ->
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

handle_cast(enter_tree, State) ->
	?INFO("enter_tree~n"),
	tree_main:enter_tree(?MODULE),
	{noreply,State}.

start() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, init, []).

enter_tree() ->
	gen_server:cast(?MODULE,enter_tree).

node1_cond(_Args) -> true.
node2_cond(_Args) -> true.
node2_cond1(_Args) -> true.
node2_cond2(_Args) -> true.
node2_cond3(_Args) -> true.
node3_cond1(_Args) -> true.
node3_cond2(_Args) -> true.
node3_cond3(_Args) -> true.
node3_cond4(_Args) -> true.

node1_action1(_Args) -> ok.
node1_action2(_Args) -> ok.
node2_action1(_Args) -> ok.
node3_action1(_Args) -> ok.
node3_action2(_Args) -> ok.