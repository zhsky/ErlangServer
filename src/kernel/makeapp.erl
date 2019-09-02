%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 16:43:30
%% @Doc:	根据make.erl修改
%% @Last:	2019-09-02 10:50:07
%% ====================================================================

-module(makeapp).

-include_lib("kernel/include/file.hrl").
-define(MakeOpts,[noexec,load,netload,noload]).
-define(BeamPath,"./ebin").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		make/2
		,update/0
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

make(Worker, Options) when Worker > 0 ->
	{MakeOpts, CompileOpts} = sort_options(Options,[],[]),
	case lists:keyfind('LOCAL_MODE',1,CompileOpts) of
		{'LOCAL_MODE',true} ->
			MAKE_FILE = 'LocalMakefile';
		_ ->
			MAKE_FILE = 'Emakefile'
	end,
	case read_emakefile(MAKE_FILE, CompileOpts) of
		Files when is_list(Files) ->
			do_make_files(Worker, Files, MakeOpts);
		error ->
			error
	end;
make(_,_) -> error.

sort_options([H|T],Make,Comp) ->
	case lists:member(H,?MakeOpts) of
	true ->
		sort_options(T,[H|Make],Comp);
	false ->
		sort_options(T,Make,[H|Comp])
	end;
sort_options([],Make,Comp) ->
	{Make,lists:reverse(Comp)}.

read_emakefile(Emakefile,Opts) ->
	case file:consult(Emakefile) of
		{ok,Emake} ->
			transform(Emake,Opts,[],[]);
		{error,enoent} ->
			%% No Emakefile found - return all modules in current 
			%% directory and the options given at command line
			Mods = [filename:rootname(F) ||	F <- filelib:wildcard("*.erl")],
			[{Mods, Opts}];
		{error,Other} ->
			io:format("make: Trouble reading 'Emakefile':~n~tp~n",[Other]),
			error
	end.

transform([{Mod,ModOpts}|Emake],Opts,Files,Already) ->
	case expand(Mod,Already) of
	[] -> 
		transform(Emake,Opts,Files,Already);
	Mods -> 
		transform(Emake,Opts,[{Mods,ModOpts++Opts}|Files],Mods++Already)
	end;
transform([Mod|Emake],Opts,Files,Already) ->
	case expand(Mod,Already) of
	[] -> 
		transform(Emake,Opts,Files,Already);
	Mods ->
		transform(Emake,Opts,[{Mods,Opts}|Files],Mods++Already)
	end;
transform([],_Opts,Files,_Already) ->
	lists:reverse(Files).

expand(Mod,Already) when is_atom(Mod) ->
	expand(atom_to_list(Mod),Already);
expand(Mods,Already) when is_list(Mods), not is_integer(hd(Mods)) ->
	lists:concat([expand(Mod,Already) || Mod <- Mods]);
expand(Mod,Already) ->
	case lists:member($*,Mod) of
	true -> 
		Fun = fun(F,Acc) -> 
				M = filename:rootname(F),
				case lists:member(M,Already) of
					true -> Acc;
					false -> [M|Acc]
				end
			end,
		lists:foldl(Fun, [], filelib:wildcard(Mod++".erl"));
	false ->
		Mod2 = filename:rootname(Mod, ".erl"),
		case lists:member(Mod2,Already) of
		true -> [];
		false -> [Mod2]
		end
	end.

do_make_files(Worker, Files, MakeOpts) ->
	process(Worker,Files, lists:member(noexec, MakeOpts), load_opt(MakeOpts)).

load_opt(Opts) ->
	case lists:member(netload,Opts) of
	true -> 
		netload;
	false ->
		case lists:member(load,Opts) of
		true ->
			load;
		_ ->
			noload
		end
	end.

process(_Worker,[], _NoExec, _Load) -> up_to_date;
process(_Worker,[{TotalTasks,Opts}|Rest], NoExec, Load) ->
	TaskSize = length(TotalTasks),
	Worker = erlang:min(TaskSize, _Worker),
	TaskPer = TaskSize div Worker,
	SplitTaskList = split_task(TotalTasks,TaskPer,[]),
	Ref = make_ref(),
	Pids = [
		process_task(Tasks,NoExec,Load,Opts,Ref,self())
		|| Tasks <- SplitTaskList
	],
	case catch process_wait(length(Pids),Ref) of
		ok ->
			process(_Worker,Rest, NoExec, Load);
		error ->
			error
	end.

split_task([], _TaskPer, Acc) ->
	lists:reverse(Acc);
split_task(Tasks, TaskPer, Acc) ->
	{L1, L2} = lists:split(erlang:min(length(Tasks), TaskPer), Tasks),
	split_task(L2, TaskPer, [L1 | Acc]).

process_task(Tasks,NoExec,Load,Opts,Ref,Super) ->
	spawn(fun() ->
		Fun = fun(H) ->
			case recompilep(coerce_2_list(H), NoExec, Load, Opts) of
				error ->
					Super ! {error, Ref},
					exit(error);
				_ ->
					ok
			end
		end,
		lists:foreach(Fun,Tasks),
		Super ! {finish, Ref}
	end).

process_wait(0,_) -> ok;
process_wait(N,Ref) -> 
	receive
		{finish,Ref} ->
			process_wait(N-1,Ref);
		{error,Ref} ->
			throw(error);
		Err ->
			io:format("~p~n",[Err]),
			throw(error)
	end.

recompilep(File, NoExec, Load, Opts) ->
	ObjName = lists:append(filename:basename(File),
				code:objfile_extension()),
	ObjFile = case lists:keysearch(outdir,1,Opts) of
		  {value,{outdir,OutDir}} ->
			  filename:join(coerce_2_list(OutDir),ObjName);
		  false ->
			  ObjName
		  end,
	case exists(ObjFile) of
	true ->
		recompilep1(File, NoExec, Load, Opts, ObjFile);
	false ->
		recompile(File, NoExec, Load, Opts)
	end.
 
recompilep1(File, NoExec, Load, Opts, ObjFile) ->
	{ok, Erl} = file:read_file_info(lists:append(File, ".erl")),
	{ok, Obj} = file:read_file_info(ObjFile),
	 recompilep1(Erl, Obj, File, NoExec, Load, Opts).

recompilep1(#file_info{mtime=Te},
		#file_info{mtime=To}, File, NoExec, Load, Opts) when Te>To ->
	recompile(File, NoExec, Load, Opts);
recompilep1(_Erl, #file_info{mtime=To}, File, NoExec, Load, Opts) ->
	recompile2(To, File, NoExec, Load, Opts).

%% recompile2(ObjMTime, File, NoExec, Load, Opts)
%% Check if file is of a later date than include files.
recompile2(ObjMTime, File, NoExec, Load, Opts) ->
	IncludePath = include_opt(Opts),
	case check_includes(lists:append(File, ".erl"), IncludePath, ObjMTime) of
	true ->
		recompile(File, NoExec, Load, Opts);
	false ->
		false
	end.

include_opt([{i,Path}|Rest]) ->
	[Path|include_opt(Rest)];
include_opt([_First|Rest]) ->
	include_opt(Rest);
include_opt([]) ->
	[].

%% recompile(File, NoExec, Load, Opts)
%% Actually recompile and load the file, depending on the flags.
%% Where load can be netload | load | noload

recompile(File, true, _Load, _Opts) ->
	io:format("Out of date: ~ts\n",[File]);
recompile(File, false, noload, Opts) ->
	io:format("Recompile: ~ts\n",[File]),
	compile:file(File, [report_errors, report_warnings, error_summary |Opts]);
recompile(File, false, load, Opts) ->
	io:format("Recompile: ~ts\n",[File]),
	c:c(File, Opts);
recompile(File, false, netload, Opts) ->
	io:format("Recompile: ~ts\n",[File]),
	c:nc(File, Opts).

exists(File) ->
	case file:read_file_info(File) of
	{ok, _} ->
		true;
	_ ->
		false
	end.

coerce_2_list(X) when is_atom(X) ->
	atom_to_list(X);
coerce_2_list(X) ->
	X.

%%% If you an include file is found with a modification
%%% time larger than the modification time of the object
%%% file, return true. Otherwise return false.
check_includes(File, IncludePath, ObjMTime) ->
	Path = [filename:dirname(File)|IncludePath], 
	case epp:open(File, Path, []) of
	{ok, Epp} ->
		check_includes2(Epp, File, ObjMTime);
	_Error ->
		false
	end.
	
check_includes2(Epp, File, ObjMTime) ->
	A1 = erl_anno:new(1),
	case epp:parse_erl_form(Epp) of
	{ok, {attribute, A1, file, {File, A1}}} ->
		check_includes2(Epp, File, ObjMTime);
	{ok, {attribute, A1, file, {IncFile, A1}}} ->
		case file:read_file_info(IncFile) of
		{ok, #file_info{mtime=MTime}} when MTime>ObjMTime ->
			epp:close(Epp),
			true;
		_ ->
			check_includes2(Epp, File, ObjMTime)
		end;
	{ok, _} ->
		check_includes2(Epp, File, ObjMTime);
	{eof, _} ->
		epp:close(Epp),
		false;
	{error, _Error} ->
		check_includes2(Epp, File, ObjMTime);
	{warning, _Warning} ->
		check_includes2(Epp, File, ObjMTime)
	end.

%%===========================update============================
update() ->
	io:format("===========================~n"),
	io:format("BEGIN UPDATE~n"),
	ModsList = get_update_lists(),
	[begin
		io:format("update ~p~n",[Mod]),
		code:purge(Mod),
		code:load_file(Mod)
	end	
	|| Mod <- ModsList],
	io:format("END UPDATE~n"),
	io:format("===========================~n").

get_update_lists() ->
	{ok,FileList} = file:list_dir(?BeamPath),
	get_update_lists(FileList,[]).

get_update_lists([],AccIn) -> AccIn;
get_update_lists([File | FileList], AccIn) ->
	try
		NewAccIn = case string:tokens(File,".") of
			[ModStr,"beam"] ->
				ModAtom = list_to_atom(ModStr),
				FileVsn = get_file_version(File),
				BeamVsn = get_beam_version(ModAtom),
				if
					FileVsn == BeamVsn->
						AccIn;
					true ->
						[ModAtom | AccIn]
				end;
			_ -> AccIn
		end,
		get_update_lists(FileList,NewAccIn)
	catch
		_:_Err -> 
			io:format("ERROR:~n~p~n",[erlang:get_stacktrace()]),
			[]
	end.


get_file_version(_File) ->
	File = filename:join(?BeamPath,_File),
	case beam_lib:version(File) of
		{ok,{_,[Vsn]}} -> Vsn;
		_Err -> 
			io:format("ERROR file version ~p:~p~n",[File,_Err]),
			throw(file_vsn_error)
	end.
get_beam_version(ModBeam) ->
	Attr = ModBeam:module_info(attributes),
	case lists:keyfind(vsn,1,Attr) of
		{vsn,[Vsn]} -> Vsn;
		_Err -> 
			io:format("ERROR beam version ~p:~p~n",[ModBeam,_Err]),
			throw(beam_vsn_error)
	end.