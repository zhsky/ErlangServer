%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-23 17:23:24
%% @Doc:	DESC
%% @Last:	2019-10-23 14:55:36
%% ====================================================================

-module(lib_npc_block).
-include("world.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		add_npc_id/3
		,del_npc_id/3
		,update_npc_position/5
		,get_area_block_obj_ids/2
		,get_square_block_obj_ids/4
		,get_block_obj_ids/1
		,get_blocks_obj_ids/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

add_npc_id(GridX,GridY,ID) ->
	SceneId = ?SCENE_ID,
	{BlockX,BlockY} = lib_block:calc_block_coord(SceneId,GridX,GridY),
	lib_block:add_obj_id(?BLOCK_NPC,BlockX,BlockY,ID).
del_npc_id(GridX,GridY,ID) ->
	SceneId = ?SCENE_ID,
	{BlockX,BlockY} = lib_block:calc_block_coord(SceneId,GridX,GridY),
	lib_block:del_obj_id(?BLOCK_NPC,BlockX,BlockY,ID).

update_npc_position(OldGridX,OldGridY,GridX,GridY,ID) ->
	del_npc_id(OldGridX,OldGridY,ID),
	add_npc_id(GridX,GridY,ID).

get_area_block_obj_ids(GridX,GridY) ->
	SceneId = ?SCENE_ID,
	{BlockX,BlockY} = lib_block:calc_block_coord(SceneId,GridX,GridY),
	lib_block:get_area_block_obj_ids(?BLOCK_NPC,BlockX, BlockY).

get_block_obj_ids(BlockId) ->
	lib_block:get_obj_ids(?BLOCK_NPC,BlockId).
get_blocks_obj_ids(BlockIds) ->
	lib_block:get_area_block_obj_ids(?BLOCK_NPC,BlockIds).

get_square_block_obj_ids(GridX1,GridY1,GridX2,GridY2) ->
	SceneId = ?SCENE_ID,
	{BlockX1,BlockY1} = lib_block:calc_block_coord(SceneId,GridX1,GridY1),
	{BlockX2,BlockY2} = lib_block:calc_block_coord(SceneId,GridX2,GridY2),
	BlockList = lib_block:get_square_block_list(SceneId,BlockX1,BlockY1,BlockX2,BlockY2),
	get_square_block_obj_ids(BlockList,[]).

get_square_block_obj_ids([],AccIn) -> AccIn;
get_square_block_obj_ids([Hd | Tail],AccIn) -> 
	get_square_block_obj_ids(Tail,lib_block:get_obj_ids(?BLOCK_NPC,Hd) ++ AccIn).
