%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-10-23 15:52:32
%% @Doc:	DESC
%% @Last:	2019-10-23 17:29:35
%% ====================================================================

-module(lib_world_player).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		player_enter/4
		,player_exit/3
		,player_move/4
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

player_enter(PlayerId,SocketPId,SceneId,Coord) ->
	fetch_area_mover_info(SocketPId, Coord),
	NotifyInfo = make_up_player_info(PlayerId),
	notify_area_mover_msg(NotifyInfo, SceneId, Coord),

	BlockId = lib_block:calc_block_by_grid(SceneId, Coord),
	lib_player_block:add_player_id(BlockId,PlayerId),

	put(PlayerId,SocketPId),
	ok.

player_exit(PlayerId,SceneId,Coord) ->
	SocketPId = get(PlayerId),
	del_area_mover_info(SocketPId, Coord),
	DelInfo = del_player_info(PlayerId),
	notify_area_mover_msg(DelInfo, SceneId, Coord),

	BlockId = lib_block:calc_block_by_grid(SceneId, Coord),
	lib_player_block:del_player_id(BlockId,PlayerId),

	erase(PlayerId),
	ok.

fetch_area_mover_info(SocketPId, {GridX,GridY} = _Coord) ->
	%%附近player信息
	PlayerList = area_player_info(GridX,GridY),
	PlayerBin = lists:list_to_binary(PlayerList),

	%%附近NPC信息
	NpcList = area_npcster_info(GridX,GridY),
	NpcBin = lists:list_to_binary(NpcList),

	gen_server:cast(SocketPId,{send,<<1001:32/little,PlayerBin/binary,NpcBin/binary>>}),
	ok.

del_area_mover_info(SocketPId, {GridX,GridY} = _Coord) ->
	%%附近player信息
	PlayerList = area_player_info(GridX,GridY),
	PlayerBin = lists:list_to_binary(PlayerList),

	%%附近NPC信息
	NpcList = area_npcster_info(GridX,GridY),
	NpcBin = lists:list_to_binary(NpcList),

	gen_server:cast(SocketPId,{send,<<1002:32/little,PlayerBin/binary,NpcBin/binary>>}),
	ok.

area_player_info(GridX,GridY) ->
	BLockIds = lib_block:calc_area_block_by_block_coord(GridX,GridY),
	block_player_info(BLockIds).
block_player_info(BLockIds) ->
	Fun = fun(BlockId,AccIn) ->
		PlayerIds = lib_player_block:get_block_obj_ids(BlockId),
		[PlayerIds | AccIn]%%TODO 根据需求打包更详细的信息
	end,
	lists:foldl(Fun,[],BLockIds).

area_npcster_info(GridX,GridY) ->
	BLockIds = lib_block:calc_area_block_by_block_coord(GridX,GridY),
	block_npc_info(BLockIds).
block_npc_info(BLockIds) ->
	Fun = fun(BlockId,AccIn) ->
		NpcIds = lib_npc_block:get_block_obj_ids(BlockId),
		[NpcIds | AccIn]%%TODO 根据需求打包更详细的信息
	end,
	lists:foldl(Fun,[],BLockIds).

make_up_player_info(PlayerId) -> 
	<<1003:32/little,PlayerId:64/little>>.

del_player_info(PlayerId) -> 
	<<1004:32/little,PlayerId:64/little>>.

player_move(PlayerId,SceneId,SrcCoord,TarCoord) ->
	{TarBlockX, TarBlockY} = lib_block:calc_block_coord(SceneId, TarCoord),
	
	{SrcGridX,SrcGridY} = SrcCoord,
	{TarGridX,TarGridY} = TarCoord,
	MoveBin = <<1005:32/little,PlayerId:64/little,SrcGridX:32/little,SrcGridY:32/little,TarGridX:32/little,TarGridY:32/little>>,

	TarBlockId = lib_block:calc_block_coord(SceneId, TarBlockX, TarBlockY),
	SrcBlockId = lib_block:calc_block_by_grid(SceneId, SrcCoord),
	case SrcBlockId =:= TarBlockId of 
		true ->
			notify_area_mover_msg(MoveBin, SceneId, TarCoord);
		false ->
			lib_player_block:del_player_id(SrcBlockId,PlayerId),
			lib_player_block:add_player_id(TarBlockId,PlayerId),

			{DelBlockIds,PubBlockIds,NewBlockIds} = lib_block:get_diff_blockids(SrcBlockId,TarBlockId),

			Bin1 = del_player_info(PlayerId),
			Bin2 = make_up_player_info(PlayerId),
			notify_area_mover_msg(Bin1,SceneId,DelBlockIds),
			notify_area_mover_msg(Bin2,SceneId,NewBlockIds),
			notify_area_mover_msg(MoveBin,SceneId,PubBlockIds),
			
			PlayerDisappear = block_player_info(DelBlockIds),
			Bin4 = lists:list_to_binary(PlayerDisappear),
			PlayerAppear = block_player_info(NewBlockIds),
			Bin5 = lists:list_to_binary(PlayerAppear),
			
			NpcDisappear = block_npc_info(DelBlockIds),
			Bin6 = lists:list_to_binary(NpcDisappear),
			NpcAppear = block_npc_info(NewBlockIds),
			Bin7 = lists:list_to_binary(NpcAppear),

			SocketPId = get(PlayerId),
			MsgBin1 = <<1002:32/little,Bin4/binary,Bin6/binary>>,
			MsgBin2 = <<1001:32/little,Bin5/binary,Bin7/binary>>,
			gen_server:cast(SocketPId,{send,<<MsgBin1/binary,MsgBin2/binary>>})
	end,
	ok.

notify_area_mover_msg(MsgBin, SceneId, Coord) ->
	{BlockX, BlockY} = lib_block:calc_block_coord(SceneId, Coord),
	BlockList = lib_block:calc_area_block_by_block_coord(BlockX, BlockY),
	PlayerIds = lib_player_block:get_blocks_obj_ids(BlockList),
	send_to_area(MsgBin, PlayerIds).

send_to_area(_MsgBin, []) -> ok;
send_to_area(MsgBin, [PlayerId | T]) ->
	SocketPId = get(PlayerId),
	gen_server:cast(SocketPId,{send,MsgBin}),
	send_to_area(MsgBin, T).