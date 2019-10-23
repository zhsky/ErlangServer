%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-25 16:06:04
%% @Doc:	DESC
%% @Last:	2019-10-23 14:55:33
%% ====================================================================

-module(lib_player_block).
-include("world.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		add_player_id/2
		,del_player_id/2
		,update_player_block/3
		,get_area_block_obj_ids/2
		,get_area_block_obj_ids/3
		,is_area_have_objs/2
		,get_blocks_obj_ids/1
		,get_block_obj_ids/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

update_player_block(BlockId,BlockId,_ID) -> skip;
update_player_block(OldBlockId, BlockId, ID) ->
	lib_block:del_obj_id(?BLOCK_PLAYER,OldBlockId,ID),
	lib_block:add_obj_id(?BLOCK_PLAYER,BlockId,ID).

add_player_id(BlockId,ID) ->
	lib_block:add_obj_id(?BLOCK_PLAYER,BlockId,ID).
del_player_id(BlockId,ID) ->
	lib_block:del_obj_id(?BLOCK_PLAYER,BlockId,ID).

get_area_block_obj_ids(GridX,GridY) ->
	SceneId = ?SCENE_ID,
	get_area_block_obj_ids(SceneId,GridX,GridY).
get_area_block_obj_ids(SceneId,GridX,GridY) ->
	{BlockX,BlockY} = lib_block:calc_block_coord(SceneId,GridX,GridY),
	lib_block:get_area_block_obj_ids(?BLOCK_PLAYER,BlockX, BlockY).

is_area_have_objs(GridX,GridY) ->
	SceneId = ?SCENE_ID,
	{BlockX,BlockY} = lib_block:calc_block_coord(SceneId,GridX,GridY),
	lib_block:is_area_have_objs(?BLOCK_PLAYER,BlockX, BlockY).

get_block_obj_ids(BlockId) ->
	lib_block:get_obj_ids(?BLOCK_PLAYER,BlockId).

get_blocks_obj_ids(BlockList) ->
	do_get_block_obj_ids(BlockList,[]).
do_get_block_obj_ids([],AccIn) -> AccIn;
do_get_block_obj_ids([BLockId | BlockList],AccIn) ->
	do_get_block_obj_ids(BlockList, lib_block:get_obj_ids(?BLOCK_PLAYER,BLockId) ++ AccIn).
