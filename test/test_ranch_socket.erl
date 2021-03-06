%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-04 18:31:20
%% @Doc:	DESC
%% @Last:	2019-09-08 19:08:47
%% ====================================================================

-module(test_ranch_socket).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		main/1
		,send_recv/0
		,socket_connect/1
		,send_data/1
		,send_big_data/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

main(Num) ->
	socket_connect(Num).

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
	timer:sleep(2000),
	gen_tcp:close(Socket).

send_big_data(_Num) ->
	SendBytes = 10 * 1024 * 1024,
	Bin = list_to_binary([1 || _ <- lists:seq(1,SendBytes)]),

	{ok, Socket} = gen_tcp:connect({192,168,16,240},6000,[binary,{active, false},{packet, 0}]),

	Id = 16#ffffff,
	Name = list_to_binary("Jack"),
	NameSize = byte_size(Name),
	Content = Bin,
	ContentLen = byte_size(Content),

	Data = <<Id:32/little,NameSize:32/little,Name/binary,ContentLen:32/little,Content/binary>>,
	DataLen = byte_size(Data),
	Msg = <<DataLen:32/little,Data/binary>>,
	gen_tcp:send(Socket,Msg),

	file:write_file("./send_file.txt",Data),
	io:format("~p,~p,~p~n",[Id,Name,ContentLen]),

	gen_tcp:close(Socket).


socket_connect(Num) ->
	statistics(wall_clock),
	Supid = self(),
	Fun = fun() ->
		case catch gen_tcp:connect({192,168,16,240},6000,[binary,{active, false}, {packet, 0}],infinity) of
			{ok, Socket} -> 
				send_data(Socket),
				gen_tcp:close(Socket),
				Supid ! finish;
			_ ->
				Supid ! false,
				skip
		end
	end,
	[spawn(Fun) || _ <- lists:seq(1,Num)],

	Ret = loop_wait(0,Num),
	io:format("false:~p~n",[Ret]),
	{_,Time} = statistics(wall_clock),
	io:format("~p~n",[Time]).

loop_wait(Failed,0) -> Failed;
loop_wait(Failed,N) ->
	receive
		finish ->
			loop_wait(Failed, N - 1);
		false ->
			loop_wait(Failed + 1,N - 1)
	end.

send_data(Socket) ->
	SendBytes = 1024,
	Bin = list_to_binary([1 || _ <- lists:seq(1,SendBytes)]),

	Id = 16#ffffff,
	Name = list_to_binary("Jack"),
	NameSize = byte_size(Name),
	Content = Bin,
	ContentLen = byte_size(Content),

	Data = <<Id:32/little,NameSize:32/little,Name/binary,ContentLen:32/little,Content/binary>>,
	DataLen = byte_size(Data),
	Msg = <<DataLen:32/little,Data/binary>>,
	gen_tcp:send(Socket,Msg),
	ok.