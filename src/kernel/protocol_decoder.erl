%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-03 21:25:08
%% @Doc:	DESC
%% @Last:	2020-02-11 16:42:04
%% ====================================================================

-module(protocol_decoder).
-behaviour(gen_server).

-include("log.hrl").

-define(CRC_HEAD_KEY, [123,0,0,0]).
-define(DES_HEAD_KEY, "abc@1234").
-define(DES_HEAD_IV, <<123:64>>).
-define(SERIAL_NUM, serial_num).
%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([
		start_link/3
	]).

-record(state,{spid,ip,port,wait_bin = <<>>,wait_len = 0,data_len = 0}).
%% ====================================================================
%% Internal functions
%% ====================================================================

start_link(SPid, IP, Port) ->
	gen_server:start_link(?MODULE, init, [SPid, IP, Port]).

init([SPid, IP, Port]) ->
	{ok,#state{spid = SPid, ip = IP, port = Port}}.

handle_info(_Info, State) ->
	{noreply,State}.

handle_call(_Info, _From, State) ->
	{noreply,State}.

terminate(_Info, #state{} = _State) ->
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

handle_cast(stop, State) ->
	{stop,normal,State};

handle_cast({decode,Data}, State) ->
	NewState = decode_data(Data,State),
	{noreply,NewState};

handle_cast(_Info, State) ->
	{noreply,State}.

decode_data(<<>>,State) -> State;
decode_data(Data,State = #state{wait_bin = WaitData,wait_len = WaitLen}) ->
	NewData = <<WaitData/binary,Data/binary>>,
	decode_waitdata(State#state{wait_bin = NewData,wait_len = WaitLen + byte_size(Data)}).

decode_waitdata(State = #state{wait_bin = <<Len:32/little,LeftData/binary>>,wait_len = WaitLen,data_len = 0}) when WaitLen >= 4 ->
	decode_waitdata(State#state{wait_bin = LeftData,wait_len = WaitLen - 4,data_len = Len});
decode_waitdata(State = #state{spid = Spid,wait_bin = WaitData,wait_len = WaitLen,data_len = DataLen}) 
		when DataLen > 0 andalso WaitLen >= DataLen ->
	case catch decode_header(WaitData,DataLen,SPid) of
		{LeftData,PackBin} ->
			decode_msg(PackBin);
		{error,LeftData} -> 
			?DEBUG("ignore illegal data")
	end,
	decode_waitdata(State#state{wait_bin = LeftData,wait_len = WaitLen - DataLen, data_len = 0});
decode_waitdata(State) -> State.

%%|4字节包长度|4字节crc32校验|8字节加密串|4字节数据体长度|数据体|
%%8节加密串由时间和包序列号经des_cbc加密
decode_header(WaitBin,Len,SPid) ->
	<<PackCrcBin:Len/binary, NewWaitBin/binary>> = WaitBin,
	<<MsgCrcCode:32/little, DesBin/binary>> = PackCrcBin,
	HeadBin = list_to_binary(?CRC_HEAD_KEY),
	CrcHeadBin = <<HeadBin/binary, DesBin/binary>>,
	CrcCode = erlang:crc32(CrcHeadBin),	
	if MsgCrcCode /= CrcCode -> ?WARNING("crc check error"), throw({error,NewWaitBin}); true -> ok end,

	<<DesCodeBin:8/binary, Packbin/binary>> = DesBin,
	<<TimeStamp:32/little,SerialNum:32/little>> = crypto:block_decrypt(des_cbc,?DES_HEAD_KEY,?DES_HEAD_IV,DesCodeBin),
	NowTime = util:unixtime(),
	if TimeStamp > NowTime orelse (NowTime - TimeStamp) > 86400 -> ?WARNING("des_cbc time check error"), throw({error,NewWaitBin}); true -> ok end,
	case validate_serial_num(SerialNum,SPid) of false -> ?WARNING("des_cbc serial num check error"), throw({error,NewWaitBin}); _ -> ok end,
	{NewWaitBin,Packbin}.

validate_serial_num(VerityNum,SPid) ->
	SerialNum = get(?SERIAL_NUM),
	if
		VerityNum =< SerialNum orelse (VerityNum - SerialNum) > 10 ->
			refresh_serial_num(SerialNum,SPid),
			false;
		true -> 
			put(?SERIAL_NUM,VerityNum),
			ok
	end.

validate_data(ClientCrcCode,FinalData) ->
	CrcBin = list_to_binary("[crc_key]"),
	Checkbin = <<CrcBin/binary,FinalData/binary>>,
	CrcCode = erlang:crc32(Checkbin),
	ClientCrcCode == CrcCode.

decode_msg(FinalData) ->
	<<Id:32/little,Data1/binary>> = FinalData,

	<<Len:32/little,Data2/binary>> = Data1,
	<<Name:Len/binary-unit:8,Data3/binary>> = Data2,

	<<ContentLen:32/little,Data4/binary>> = Data3,
	<<Content:ContentLen/binary-unit:8,_Data/binary>> = Data4,

	?INFO("~p,~p,~p",[Id,Name,byte_size(Content)]).