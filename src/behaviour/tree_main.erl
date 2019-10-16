%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-18 10:58:01
%% @Doc:	DESC
%% @Last:	2019-09-24 10:27:28
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

enter_tree(InputParam = #input_param{tree = Node,args = Args}) ->
	case catch enter_node(Node,Args) of
		false -> InputParam;
		#tree_node{} = NewNode -> InputParam#input_param{tree = NewNode};
		_Err -> ?WARNING("ERROR ~p ~n",[_Err]),InputParam
	end.

%% @Ret updated current node | false
enter_node(undefined,_) -> false;	
enter_node(Node,Args) ->
	#tree_node{node_type = NodeType,node_precondition = PreCond} = Node,
	case PreCond == undefined orelse condition_node:is_true(PreCond,Args) of
		true -> execute_node(NodeType, Node, Args);
		_ -> false
	end.

%% @Ret updated current node
execute_node(?CONTROL, #tree_node{node_action = ?PRIOSELNODE,sub_node = SubNodes} = Node, Args) -> 
	NewNode = case SubNodes == [] of
		true -> Node;
		_ ->
			[FirstNode | LeftNodes] = SubNodes,
			case enter_node(FirstNode,Args) of
				false -> 
					TmpSubNode = Node#tree_node{sub_node = LeftNodes},
					#tree_node{sub_node = NewLeftNodes} = execute_node(?CONTROL,TmpSubNode,Args),
					Node#tree_node{sub_node = [FirstNode | NewLeftNodes]};
				UpdateFirstNode ->
					Node#tree_node{sub_node = [UpdateFirstNode | LeftNodes]}
			end
	end,
	NewNode;

execute_node(?CONTROL, #tree_node{node_action = ?SEQUENCENODE,sub_node = SubNodes, 
		sequence_index = Index,sequence_length = Length} = Node, Args) -> 
	CurNode = lists:nth(Index,SubNodes),
	NewNode = case enter_node(CurNode,Args) of
		false -> 
			Node;
		NewCurNode ->
			NewSubNodes = lists:keyreplace(NewCurNode#tree_node.node_id,#tree_node.node_id,SubNodes,NewCurNode),
			Node#tree_node{sub_node = NewSubNodes,sequence_index = Index rem Length + 1}
	end,
	NewNode;
execute_node(?CONTROL, #tree_node{node_action = ?PARASELNODE,sub_node = SubNodes} = Node, Args) -> 
	NewSubNodes = [
		begin
			case enter_node(ExecNode,Args) of
				false -> 
					UpdateExecNode = ExecNode;
				UpdateExecNode -> ok
			end,
			UpdateExecNode
	end|| ExecNode <- SubNodes],
	Node#tree_node{sub_node = NewSubNodes};
execute_node(?ACTION, #tree_node{node_action = ActionNode} = Node, Args) -> 
	action_node:execute(ActionNode,Args),
	Node;
execute_node(_NodeType, Node, _Args) -> 
	?WARNING("ERROR execute_node~n"),
	Node.
