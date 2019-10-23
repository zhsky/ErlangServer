%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-23 16:13:05
%% @Doc:	DESC
%% @Last:	2019-10-23 17:20:35
%% ====================================================================

-module(lib_block).
-include("world.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		calc_block_by_grid/2,
		calc_block_by_grid/3,
		calc_block_coord/2,
		calc_block_coord/3,
		calc_area_block/1,
		calc_area_block_by_block_coord/2,
		get_area_block_obj_ids/2,
		get_area_block_obj_ids/3,
		add_obj_id/3,
		add_obj_id/4,
		del_obj_id/3,
		del_obj_id/4,
		get_obj_ids/2,
		get_square_block_list/5,
		is_area_have_objs/3,
		get_diff_blockids/2,
		is_have_obj/2,
		is_same_block/5
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================


calc_block_by_grid(SceneId, [GridX, GridY | _T]) ->
	calc_block_by_grid(SceneId, GridX, GridY).
calc_block_by_grid(SceneId, GridX, GridY) ->
	{BlockX, BlockY} = calc_block_coord(SceneId, GridX, GridY),
	calc_block_id(BlockX, BlockY).
calc_block_id(BlockX, BlockY) ->
	BlockWidthAmount = get(?BWA),
	BlockY * BlockWidthAmount + BlockX.

calc_block_coord(SceneId, [GridX, GridY | _T]) ->
	calc_block_coord(SceneId, GridX, GridY).
calc_block_coord(_SceneId, GridX, GridY) ->
	BlockWidth = ?BW,
	BlockHeight = ?BH,
	BlockX = GridX div BlockWidth,
	BlockY = GridY div BlockHeight,
	{BlockX, BlockY}.

%% 计算九宫格的区块索引值
calc_area_block_by_block_coord(BlockX, BlockY) ->
	BlockId = calc_block_id(BlockX, BlockY),
	calc_area_block(BlockId).
calc_area_block(BlockId) ->
	BlockWidthAmount = get(?BWA),
	L = [
		BlockId - 1 - BlockWidthAmount,	BlockId - BlockWidthAmount,	BlockId + 1 - BlockWidthAmount,
		BlockId - 1,					BlockId,					BlockId + 1,
		BlockId - 1 + BlockWidthAmount,	BlockId + BlockWidthAmount,	BlockId + 1 + BlockWidthAmount
	],

	MaxBlckokId = get(?MAX_BLOCK_ID),
	lists:filter(fun(Id) -> Id >= 0 andalso Id < MaxBlckokId end,L).

get_area_block_obj_ids(Type,BlockX, BlockY) ->
	BlockIds = calc_area_block_by_block_coord(BlockX, BlockY),
	do_get_area_block_obj_ids(Type,BlockIds,[]).
get_area_block_obj_ids(Type,BlockIds) ->
	do_get_area_block_obj_ids(Type,BlockIds,[]).

do_get_area_block_obj_ids(_Type,[],AccIn) -> AccIn;
do_get_area_block_obj_ids(Type,[BlockId | BlockIds],AccIn) ->
	L = get_obj_ids(Type,BlockId),
	do_get_area_block_obj_ids(Type,BlockIds,L ++ AccIn).

%计算矩形区块
adjust_square_position(_SceneId,BlockX1,BlockY1,BlockX2,BlockY2) ->
	BlockWidthAmount = ?BWA,
	BlockHeightAmount = ?BHA,
	BlockX11 = if 
		BlockX1 >= BlockWidthAmount -> BlockWidthAmount - 1;
		BlockX1 < 0 -> 0;
		true -> BlockX1
	end,
	BlockX21 = if 
		BlockX2 >= BlockWidthAmount -> BlockWidthAmount - 1;
		BlockX2 < 0 -> 0;
		true -> BlockX2
	end,
	BlockY11 = if 
		BlockY1 >= BlockHeightAmount -> BlockHeightAmount - 1;
		BlockY1 < 0 -> 0;
		true -> BlockY1
	end,
	BlockY21 = if 
		BlockY2 >= BlockHeightAmount -> BlockHeightAmount - 1;
		BlockY2 < 0 -> 0;
		true -> BlockY2
	end,
	{BlockX11,BlockY11,BlockX21,BlockY21}.

get_square_block_list(SceneId,_BlockX1,_BlockY1,_BlockX2,_BlockY2) ->
	{BlockX1,BlockY1,BlockX2,BlockY2} = adjust_square_position(SceneId,_BlockX1,_BlockY1,_BlockX2,_BlockY2),
	MinBlockId = calc_block_id(BlockX1,BlockY1),
	MaxBlockId = calc_block_id(BlockX2,BlockY2),
	WidthAmount = get(?BWA),
	case MinBlockId of
		MaxBlockId -> 
			[MinBlockId];
		_ ->
			[YGrid * WidthAmount + XGrid ||
				XGrid <- lists:seq(MinBlockId rem WidthAmount,MaxBlockId rem WidthAmount),
				YGrid <- lists:seq(MinBlockId div WidthAmount,MaxBlockId div WidthAmount)]
	end.

add_obj_id(Type,BlockX,BlockY,Id) ->
	BlockId = calc_block_id(BlockX,BlockY),
	add_obj_id(Type,BlockId,Id).
add_obj_id(Type,BlockId,Id) ->
	L = get_obj_ids(Type,BlockId),
	L1 = [Id | lists:delete(Id,L)],
	put_obj_ids(Type,BlockId,L1).
del_obj_id(Type,BlockX,BlockY,Id) ->
	BlockId = calc_block_id(BlockX,BlockY),
	del_obj_id(Type,BlockId,Id).
del_obj_id(Type,BlockId,Id) ->
	L = get_obj_ids(Type,BlockId),
	L1 = lists:delete(Id,L),
	put_obj_ids(Type,BlockId,L1).

get_obj_ids(Type,BlockId) ->
	case get({Type,BlockId}) of
		undefined -> [];
		L -> L
	end.
put_obj_ids(Type,BlockId,L) ->
	put({Type,BlockId},L).


is_area_have_objs(Type,BlockX, BlockY) ->
	BlockIds = calc_area_block_by_block_coord(BlockX, BlockY),
	Fun = fun(BlockId) ->
		get_obj_ids(Type,BlockId) /= []
	end,
	lists:any(Fun,BlockIds).

%% @doc 获取两个有序区块List的差别IDS {Old,Public,New}
get_diff_blockids(BlockId1,BlockId2) ->
	BlockIds1 = calc_area_block(BlockId1),
	BlockIds2 = calc_area_block(BlockId2),
	do_diff_ids(BlockIds1,BlockIds2,[],[],[]).
do_diff_ids([E1 | L1],[E2 | _] = L2,L,M,R) when E1 < E2 ->
	do_diff_ids(L1,L2,[E1 | L],M,R);
do_diff_ids([E1 | L1],[E1 | L2],L,M,R) -> %% E1 == E2
	do_diff_ids(L1,L2,L,[E1 | M],R);
do_diff_ids([_ | _] = L1,[E2 | L2],L,M,R) -> %% E1 > E2
	do_diff_ids(L1,L2,L,M,[E2 | R]);
do_diff_ids([],[_|_] = L2,L,M,R) -> {L,M,L2 ++ R};
do_diff_ids([_|_] = L1,[],L,M,R) -> {L1 ++ L,M,R};
do_diff_ids([],[],L,M,R) -> {L,M,R}.

is_have_obj(GridX, GridY) ->
	BlockId = calc_block_by_grid(?SCENE_ID,GridX, GridY),
	Flag = get_obj_ids(?BLOCK_NPC,BlockId),
	FinalFlag = Flag orelse get_obj_ids(?BLOCK_PLAYER,BlockId),

	FinalFlag.

is_same_block(SceneId,OldGridX,OldGridY,GridX,GridY) ->
	{BlockX1, BlockY1} = calc_block_coord(SceneId, OldGridX, OldGridY),
	{BlockX2, BlockY2} = calc_block_coord(SceneId, GridX, GridY),
	calc_block_id(BlockX1,BlockY1) == calc_block_id(BlockX2, BlockY2).