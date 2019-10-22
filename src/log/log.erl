%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-02 14:27:11
%% @Doc:	DESC
%% @Last:	2019-09-02 17:39:42
%% ====================================================================

-module(log).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	debug_msg/4
	,info_msg/4
	,warning_msg/4
	,error_msg/4
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================
-ifdef (DEBUG_MODE).
debug_msg(Module, Line, Format, Args) ->
	notify(info_msg, "[DEBUG][~p,~p:~p]: " ++ Format, [self(), Module, Line] ++ Args).
-else.
debug_msg(_,_,_,_) -> ok.
-endif.

info_msg(Module, Line, Format, Args) ->
	notify(info_msg, "[~p,~p:~p]: " ++ Format, [self(), Module, Line] ++ Args).

warning_msg(Module, Line, Format, Args) ->
	notify(warning_msg, "[~p,~p:~p]: " ++ Format, [self(), Module, Line] ++ Args).

error_msg(Module, Line, Format, Args) ->
	notify(error, "[~p,~p:~p]: " ++ Format, [self(), Module, Line] ++ Args).

notify(Type, Format, Args) ->
	LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
	gen_event:notify(error_logger, LoggerMsg).