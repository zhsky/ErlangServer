%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-04 18:31:20
%% @Doc:	DESC
%% @Last:	2019-09-04 22:07:22
%% ====================================================================

-module(test_ranch_socket).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		main/1
		,send_recv/0
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

main(Num) ->
	socket_connect(Num).

socket_connect(Num) ->
	Pids = [
		begin
			{ok, Socket} = gen_tcp:connect({192,168,16,240},6000,[binary,{active, once}, {packet, 0}]),
			Socket
		end
	|| _ <- lists:seq(1,Num)
	],
	[gen_tcp:close(Socket) || Socket <- Pids].
	

send_recv() ->
	{ok, Socket} = gen_tcp:connect({192,168,16,240},6000,[binary,{active, once}, {packet, 0}]),
	Fun = fun(Loop) ->
		case gen_tcp:recv(Socket,0) of
			 {ok, Packet} -> 
			 	io:format("recv size:~p~n",[byte_size(Packet)]);
			 {error, closed} ->
			 	exit(normal);
			 {error, Reason} ->
			 	io:format("recv error:~p~n",[Reason])
		end,
		Loop(Loop)
	end,
	spawn(fun() ->
		Fun(Fun)
	end),
	lists:foreach(fun(_) ->
		gen_tcp:send(Socket,erlang:pid_to_list(self()))
	end,lists:seq(1,500)),
	receive after 2000 -> ok end,
	gen_tcp:close(Socket).

