%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-02 11:00:39
%% @Doc:	根据error_logger_file_h修改
%% @Last:	2019-09-05 12:21:14
%% ====================================================================

-module(logger_handle).
-behaviour(gen_event).

-record(st, {fd, filename}).
-define(FILE_OPTION,[append, raw,{delayed_write, 64 * 1024, 500}]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		init/1
		,handle_event/2
		,handle_call/2
		,handle_info/2
		,terminate/2
		,code_change/3
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

init(_Args) ->
	case new_log(#st{}) of
		State = #st{} ->
			{ok,State};
		Error ->
			Error
	end.

new_log(#st{fd = PreFd} = State) ->
	BaseDir	= "./log/",
	filelib:ensure_dir(BaseDir),
	File = get_log_name(BaseDir),
	case file:open(File, ?FILE_OPTION) of
		{ok,Fd} ->
			file:sync(PreFd),
			file:close(PreFd),
			cron_next_log(),
			State#st{fd = Fd,filename = File};
		_Error ->
			State
	end.

get_log_name(_BaseDir) ->
	BaseDir = filename:join(_BaseDir,erlang:atom_to_list(node()) ++ "_"),
	{{Y, M, D},{H,_,_}} = erlang:localtime(),
	DateFormat = lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w-~2.2.0w",[Y,M,D,H])),
	lists:concat([BaseDir,DateFormat,".log"]).

cron_next_log() ->
	NextTime = next_hour_time(),
	erlang:spawn(fun() ->
		timer:sleep(NextTime * 1000),
		gen_event:notify(error_logger, next_log)
	end).

next_hour_time() ->
	{_H,M,S} = erlang:time(),
	case M == 59 andalso S > 55 of
		true ->
			3600 + 60 - S;
		false ->
			3600 - (M * 60 + S)
	end.

handle_event(next_log, State) ->
	State2 = new_log(State),
	{ok,State2};
handle_event(Event, State) ->
	write_event(State, Event),
	{ok, State}.
handle_info({'EXIT', _Fd, _Reason}, _State) ->
	remove_handler;
handle_info(_, State) ->
	{ok, State}.
handle_call(_Query, State) ->
	{ok, {error, bad_query}, State}.
terminate(_Reason, #st{fd=Fd}) ->
	file:close(Fd).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

write_event(State, {error, _GL, Msg}) ->
	do_write_event(State,Msg,"E");

write_event(State, {info_msg, _GL, Msg}) ->
	do_write_event(State,Msg,"I");

write_event(State, {warning_msg, _GL, Msg}) ->
	do_write_event(State,Msg,"W");

write_event(State, {error_report, _GL, {Pid, std_error, _Args}}) ->
	Args = format_term(_Args),
	do_write_event(State,{Pid,std_error,Args},"ERROR_REPORT");

write_event(State, {info_report, _GL, {Pid, std_info, _Args}}) ->
	Args = format_term(_Args),
	do_write_event(State,{Pid,std_info,Args},"INFO_REPORT");

write_event(State, {warning_report, _GL, {Pid, std_warning, _Args}}) ->
	Args = format_term(_Args),
	do_write_event(State,{Pid,std_warning,Args},"WARNING_REPORT");

write_event(_, _) ->ok.

do_write_event(#st{fd = Fd}, {Pid, _Format, Args},Head) ->
	Header = write_time(erlang:localtime(), Head),
	Format = at_node(node(Pid)) ++ _Format,
	case catch lists:flatten(io_lib:format(Format, Args)) of
		Body when is_list(Body) ->
			file:write(Fd,[Header,Body]);
		_ERR ->
			Err = io_lib:format("ERROR:~p--~p,~p" ,[Format,Args,_ERR]),
			file:write(Fd,[Header,Err])
	end.

at_node(Node) when Node /= node() ->
	io_lib:format("** at node ~p **~n",[Node]);
at_node(_Node) -> [].

format_term(Term) when is_list(Term) ->
	case string_p(Term) of
	true ->
		[{"~s\n",[Term]}];
	false ->
		format_term_list(Term)
	end;
format_term(Term) ->
	[{"~p\n",[Term]}].

format_term_list([{Tag,Data}|T]) ->
	[{"    ~p: ~p\n",[Tag,Data]}|format_term_list(T)];
format_term_list([Data|T]) ->
	[{"    ~p\n",[Data]}|format_term_list(T)];
format_term_list([]) ->
	[];
format_term_list(_) ->
	%% Continue to allow non-proper lists for now.
	%% FIXME: Remove this clause in OTP 19.
	[].

string_p([]) ->
	false;
string_p(Term) ->
	string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
	string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
	string_p1(H) andalso string_p1(T);
string_p1([]) -> true;
string_p1(_) ->  false.

write_time({_,{H,M,S}}, Type) ->
	lists:flatten(io_lib:format("~n(~.2.0w:~.2.0w:~.2.0w)[~s]",[H, M, S, Type])).

