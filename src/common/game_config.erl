%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 14:16:10
%% @Doc:	DESC
%% @Last:	2019-09-01 20:12:16
%% ====================================================================

-module(game_config).
-define(MOD_CONF,mod_conf).
-define(CONF_FILE,"config/game.config").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		init/0
		,get/1
		,get/2
		,get_list/0
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

init() ->
	{ok,TermList} = file:consult(?CONF_FILE),
	compile_config:compile(?MOD_CONF,TermList).

get(Key) ->
	get(Key,undefined).

get(Key,Default) ->
	compile_config:get(?MOD_CONF,Key,Default).

get_list() ->
	compile_config:get_list(?MOD_CONF).
	