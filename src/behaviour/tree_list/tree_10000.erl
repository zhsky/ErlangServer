%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-17 18:13:40
%% @Doc:	测试demo
%% @Last:	2019-10-16 17:11:08
%% ====================================================================

-module(tree_10000).
%% >>>>=============================不变===========================
-include("behaviour.hrl").
-export([tree_info/0]).
tree_info() ->
%% <<<<=============================不变===========================
%% =============================以下定义行为树======================
[
	{node_name, <<"测试demo">>},
	{node_type, ?CONTROL},
	{node_action, ?PRIOSELNODE},
	{node_tree,
	[
		[
			{node_name, <<"节点一">>},
			{node_cond, node1_cond},
			{node_type, ?CONTROL},
			{node_action, ?SEQUENCENODE},
			{node_tree, 
			[
				[
					{node_name, <<"节点一行为1">>},
					{node_type, ?ACTION},
					{node_action, node1_action1}
				],
				[
					{node_name, <<"节点一行为2">>},
					{node_type, ?ACTION},
					{node_action, node1_action2}
				]
			]}
		],
		[
			{node_name, <<"节点二">>},
			{node_cond, node2_cond},
			{node_type, ?CONTROL},
			{node_action, ?PARASELNODE},
			{node_tree, 
			[
				[
					{node_name, <<"节点二行为1">>},
					{node_cond, node2_cond1},
					{node_type, ?ACTION},
					{node_action, node2_action1}
				],
				[
					{node_name, <<"节点二行为2">>},
					{cond_type, ?OR},
					{node_cond, [node2_cond2,node2_cond3]},
					{node_type, ?CONTROL},
					{node_action, ?PRIOSELNODE},
					{node_tree, 
					[
						[
							{node_name, <<"节点三行为1">>},
							{cond_type, ?AND},
							{node_cond,[node3_cond1, node3_cond2]},
							{node_type, ?ACTION},
							{node_action, node3_action1}
						],
						[
							{node_name, <<"节点三行为2">>},
							{cond_type, ?OR},
							{node_cond,[node3_cond3, node3_cond4]},
							{node_type, ?ACTION},
							{node_action, node3_action2}
						]
					]}
				]
			]}
		]
	]}
].