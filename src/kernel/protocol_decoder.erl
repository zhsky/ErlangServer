%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-03 21:25:08
%% @Doc:	DESC
%% @Last:	2019-09-09 10:59:09
%% ====================================================================

-module(protocol_decoder).
-behaviour(gen_server).

-include("log.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([
		start_link/0
	]).

-record(state,{wait_bin = <<>>,wait_len = 0,data_len = 0}).
%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?MODULE, init, []).

init(_) ->
	{ok,#state{}}.

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
decode_waitdata(State = #state{wait_bin = WaitData,wait_len = WaitLen,data_len = DataLen}) 
		when DataLen > 0 andalso WaitLen >= DataLen ->
	DataLen1 = DataLen - 4,
	<<CrcCode:32/little,FinalData:DataLen1/binary,LeftData/binary>> = WaitData,

	case validate_data(CrcCode,FinalData) of
		true ->
			decode_msg(FinalData);
		_ ->
			?DEBUG("ignore illegal data")
	end,
	decode_waitdata(State#state{wait_bin = LeftData,wait_len = WaitLen - DataLen, data_len = 0});
decode_waitdata(State) -> State.

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