%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-18 10:57:35
%% @Doc:	DESC
%% @Last:	2019-10-16 21:04:53
%% ====================================================================

-module(tree_manager).
-include("behaviour.hrl").
-include("log.hrl").

-define(QUIT_IF(A,Msg),case A of true -> throw(Msg);_ -> skip end).
-define(QUIT_IF_NOT(A,Msg),case A of false -> throw(Msg);_ -> skip end).
-define(CONTROL_SET,[?PRIOSELNODE,?SEQUENCENODE,?PARASELNODE]).
-define(COND_SET,[?DEFAULT,?OR,?AND]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([get_tree/1,update_tree/1]).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_tree(TreeName) ->
	case get(TreeName) of
		undefined ->
			TreeNode = start_build(TreeName),
			put(TreeName,TreeNode),
			TreeNode;
		TreeNode -> TreeNode
	end.

start_build(TreeName) ->
	try 
		Node = TreeName:tree_info(),
		_TreeNode = build_tree(Node),
		TreeNode = validate_tree(TreeName,_TreeNode),
		TreeNode
	catch
		_:Err:Trace ->
			?ERROR("~p:~p~n",[Err,Trace]),
			undefined
	end.

update_tree([]) -> ok;
update_tree([TreeName | Tail]) -> 
	put(TreeName,undefined),
	update_tree(Tail).

build_tree(undefined) -> [];
build_tree(Node) ->
	NodeType = proplists:get_value(node_type,Node),
	NodeId = get_node_id(),
	NodeAction = proplists:get_value(node_action,Node),

	NodeCond = proplists:get_value(node_cond,Node),
	CondType = proplists:get_value(cond_type,Node,?DEFAULT),
	SubNodes = proplists:get_value(node_tree,Node,[]),

	case undefined == NodeType orelse undefined == NodeId orelse undefined == NodeAction of
		true ->
			?WARNING("~p,undefined",[{Node,NodeType,NodeId,NodeAction}]),
			undefined;
		_ ->
			if
				NodeAction == ?SEQUENCENODE andalso is_list(SubNodes)-> SequenceLength = length(SubNodes);
				true -> SequenceLength = 1
			end,

			Cond = #node_cond{cond_type = CondType, cond_list = NodeCond},
			SubNode = [build_tree(_SubNode) || _SubNode <- SubNodes],
			FinalNode = #tree_node{node_id = NodeId, node_type = NodeType, node_precondition = Cond, 
				sequence_length = SequenceLength, node_action = NodeAction, sub_node = SubNode},
			FinalNode
	end.

validate_tree(TreeName,TreeNode) ->
	?INFO("validate_tree:~p",[TreeName]),
	case catch do_validate_tree(TreeNode) of
		{error,Err} ->
			?WARNING("ERROR ~p,Name:~p,Tree:~p",[Err,TreeName,TreeNode]),
			undefined;
		ok -> TreeNode;
		Err -> 
			?WARNING("ERROR ~p,Name:~p,Tree:~p",[Err,TreeName,TreeNode]),
			undefined
	end.

do_validate_tree(#tree_node{node_id = NodeId, node_type = NodeType, node_precondition = NodeCond,
		node_action = NodeAction, sequence_length = Length, sub_node = SubNodes}) ->
	?INFO("validate_node_id:~p",[NodeId]),

	#node_cond{cond_type = CondType} = NodeCond,
	?QUIT_IF_NOT(lists:member(CondType,?COND_SET),{error,cond_type}),
	
	case NodeType of
		?ACTION ->
			?QUIT_IF_NOT(is_atom(NodeAction),{error,action_node1});
		?CONTROL ->
			?QUIT_IF_NOT(lists:member(NodeAction,?CONTROL_SET),{error,action_node2}),
			?QUIT_IF(length(SubNodes) == 0,{error,empty_subnode});
		_ ->
			throw({error,wrong_node_type})
	end,

	case {NodeType,NodeAction} of
		{?CONTROL,?SEQUENCENODE} ->
			SubLen = length(SubNodes),
			?QUIT_IF(Length > SubLen,{error,wrong_subnode_length});
		_ -> ok
	end,

	lists:foreach(fun(SubNode) -> 
		do_validate_tree(SubNode) 
	end,SubNodes).

get_node_id() ->
	case get(node_id) of
		undefined -> 
			put(node_id,1),
			1;
		Id -> 
			put(node_id,Id + 1),
			Id + 1
	end.