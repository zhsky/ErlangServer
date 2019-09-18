%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-18 09:21:27
%% @Doc:	DESC
%% @Last:	2019-09-18 10:51:58
%% ====================================================================

-module(tree_test).
-behaviour(gen_server).
-include("log.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([
		start/0
		,get_node/0
		,set_node/1
		,value/0
		,enter_tree/0
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

init(_) ->
	Node = tree_manager:get_tree(tree_10000),
	set_node(Node),
	tree_main:enter_tree(?MODULE),
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

get_node() ->
	get(tree_node).

set_node(Node) ->
	put(tree_node,Node).

value() -> 100.