%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-18 11:01:06
%% @Doc:	behaviour
%% @Last:	2019-09-20 09:30:26
%% ====================================================================

-define(ACTION,1).%%节点类型，动作节点
-define(CONTROL,2).%%节点类型，控制节点
-define(PRIOSELNODE,1).%%带优先级的选择节点,从第一个节点开始搜索到一个可执行的子节点后就停止搜索后续子节点
-define(SEQUENCENODE,2).%%将其所有子节点依次执行，只有当前一个返回“完成”状态后，再运行下一个子节点
-define(PARASELNODE,3).%%将其所有子节点都运行一遍，无论前一个节点返回结果如何

-define(DEFAULT,0).
-define(OR,1).
-define(AND,2).

-record(input_param,{
		tree,
		args
	}).

-record(tree_node,{node_id,
		node_type,
		node_precondition,
		node_action,
		sequence_index = 1,
		sequence_length = 1,
		sub_node = []}).

-record(node_cond,{
		cond_type = 0,
		cond_list = []
	}).