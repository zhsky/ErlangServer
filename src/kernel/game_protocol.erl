%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-03 17:08:14
%% @Doc:	基于ranch库开发,增加了ranch_tcp对异步收发消息的支持
%% @Last:	2019-09-03 21:45:35
%% ====================================================================

-module(game_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("log.hrl").

-record(state, {socket, ip, port, transport,dpid}).
%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([
		start_link/3
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link(Ref, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}])}.

init({Ref, Transport, _Opts}) ->
	{ok, Socket} = ranch:handshake(Ref),
	{ok, {IP, Port}} = Transport:peername(Socket),
	{ok,DPid} = protocol_decoder:start_link(),
	Transport:async_recv(Socket, 0, infinity),
	gen_server:enter_loop(?MODULE,[],#state{socket = Socket, ip = esockd_net:format_ip(IP), port = Port, transport = Transport,dpid = DPid}).

handle_info({inet_async, Socket, _Ref, {ok,DataBin}}, #state{socket = Socket, transport = Transport,dpid = DPid} = State) ->
	gen_server:cast(DPid,{decode,DataBin}),
	Transport:async_recv(Socket, 0, infinity),
	{noreply,State};
handle_info({inet_async, Socket, _Ref, {error,Reason}}, #state{socket = Socket, ip = IP, port = Port} = State) ->
	?ERROR_MSG("RECV ERROR ~p:~w,~p",[IP,Port,Reason]),
	{stop, {recv_error,Reason}, State};

handle_info({inet_reply, _Socket ,ok}, State) ->
	{noreply, State};
handle_info({inet_reply, Socket, {error, Reason}}, #state{socket = Socket, ip = IP, port = Port} = State) ->
	?ERROR_MSG("SEND ERROR ~p:~w,~p",[IP,Port,Reason]),
	{stop, {send_error,Reason}, State};

handle_info({tcp_error, Socket, Reason}, #state{socket = Socket, ip = IP, port = Port} = State) ->
	?ERROR_MSG("tcp error ~p:~w ~n", [IP, Port]),
	{stop, {tcp_error,Reason}, State};
handle_info({tcp_closed, Socket}, #state{socket = Socket, ip = IP, port = Port} = State) ->
	?ERROR_MSG("tcp close ~p:~w ~n", [IP, Port]),
	{stop, normal, State};

handle_info({send, DataBin}, #state{socket = Socket, ip = IP, port = Port, transport = Transport} = State) ->
	case Transport:async_send(Socket, DataBin) of
		ok -> ok;
		Error ->
			?ERROR_MSG("async_send ~p:~w ~p~n", [IP, Port,Error])
	end,
	{noreply, State};

handle_info(_Info, State) ->
	{noreply,State}.

handle_call(_Info, _From, State) ->
	{noreply,State}.

handle_cast(_Info, State) ->
	{noreply,State}.

terminate(_Info, #state{socket = Socket,dpid = DPid} = _State) ->
	gen_server:cast(DPid,stop),
	catch erlang:port_close(Socket),ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

