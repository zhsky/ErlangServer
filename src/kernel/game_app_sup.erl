%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 19:50:47
%% @Doc:	DESC
%% @Last:	2019-09-03 18:10:41
%% ====================================================================

-module(game_app_sup).
-behaviour(supervisor).

-define(CHILD_SPEC(Mod),{Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
-define(SUP_FLAGS,{one_for_one,1,60}).

%% ranch default options:
%% {backlog,1024},{nodelay,true},{send_timeout,30000},{send_timeout_close,true},binary, {active, false}, {packet, raw}, {reuseaddr, true}
%% forbid custom options:
%% active, header, mode, packet, packet_size, line_delimiter, reuseaddr
%% ranch starts a pool contains 10 sockets acceptor and all listen on 1 socket default
-define(TCP_OPTIONS, [
	%%{packet, raw},
	%%{reuseaddr, true},
	%%{num_acceptors,10},
	%%{num_conns_sups,10},
	{num_listen_sockets,5},
	{recbuf, 64 * 1024},
	{sndbuf, 64 * 1024},
	{high_watermark, 64 * 1024},
	{delay_send, true}
]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		start_link/0
		,init/1
		,start_child/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
	Ip = game_config:get(srever_ip),
	Port = game_config:get(server_port),
	RanchSpec = ranch:child_spec({?MODULE,game_protocol},ranch_tcp,
			#{socket_opts => [{ip, Ip},{port, Port}] ++ ?TCP_OPTIONS},
			game_protocol,[]),

	StartList = [RanchSpec],
	{ok,{?SUP_FLAGS, StartList}}.

start_child(Mod) ->
	supervisor:start_child(?MODULE, ?CHILD_SPEC(Mod)).