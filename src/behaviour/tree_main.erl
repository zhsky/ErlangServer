%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-17 10:22:55
%% @Doc:	DESC
%% @Last:	2019-09-18 09:30:38
%% ====================================================================

-module(tree_main).
-include("behaviour.hrl").
-include("log.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([enter_tree/1]).

%% ====================================================================
%% Internal functions
%% ====================================================================

enter_tree(InputParam) ->
	Node = InputParam:get_node(),
	case catch enter_node(Node,InputParam) of
		false -> ok;
		#tree_node{} = NewNode -> InputParam:set_node(NewNode);
		_Err -> ?WARNING("ERROR ~p ~n",[_Err]),ok
	end.

%% @Ret updated current node | false
enter_node(Node,InputParam) ->
	#tree_node{node_type = NodeType,node_precondition = PreCond} = Node,
	case PreCond == undefined orelse condition_node:is_true(PreCond,InputParam) of
		true -> execute_node(NodeType, Node, InputParam);
		_ -> false
	end.

%% @Ret updated current node
execute_node(?CONTROL, #tree_node{node_action = ?PRIOSELNODE,sub_node = SubNodes} = Node, InputParam) -> 
	NewNode = case SubNodes == [] of
		true -> Node;
		_ ->
			[FirstNode | LeftNodes] = SubNodes,
			case enter_node(FirstNode,InputParam) of
				false -> 
					TmpSubNode = Node#tree_node{sub_node = LeftNodes},
					#tree_node{sub_node = NewLeftNodes} = execute_node(?CONTROL,TmpSubNode,InputParam),
					Node#tree_node{sub_node = [FirstNode | NewLeftNodes]};
				UpdateFirstNode ->
					Node#tree_node{sub_node = [UpdateFirstNode | LeftNodes]}
			end
	end,
	NewNode;

execute_node(?CONTROL, #tree_node{node_action = ?SEQUENCENODE,sub_node = SubNodes, 
		sequence_index = Index,sequence_length = Length} = Node, InputParam) -> 
	CurNode = lists:nth(Index,SubNodes),
	NewNode = case enter_node(CurNode,InputParam) of
		false -> 
			Node;
		NewCurNode ->
			NewSubNodes = lists:keyreplace(NewCurNode#tree_node.node_id,#tree_node.node_id,SubNodes,NewCurNode),
			Node#tree_node{sub_node = NewSubNodes,sequence_index = Index rem Length + 1}
	end,
	NewNode;
execute_node(?CONTROL, #tree_node{node_action = ?PARASELNODE,sub_node = SubNodes} = Node, InputParam) -> 
	NewSubNodes = [
		begin
			case enter_node(ExecNode,InputParam) of
				false -> 
					UpdateExecNode = ExecNode;
				UpdateExecNode -> ok
			end,
			UpdateExecNode
	end|| ExecNode <- SubNodes],
	Node#tree_node{sub_node = NewSubNodes};
execute_node(?ACTION, #tree_node{node_action = ActionNode} = Node, InputParam) -> 
	action_node:execute(ActionNode,InputParam),
	Node;
execute_node(_NodeType, Node, _InputParam) -> 
	?WARNING("ERROR execute_node~n"),
	Node.
