%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 16:43:30
%% @Doc:	根据make.erl修改
%% @Last:	2019-09-17 21:08:43
%% ====================================================================

-module(makeapp).

-include_lib("kernel/include/file.hrl").
-define(MakeOpts,[noexec,load,netload,noload]).
-define(BeamPath,"./ebin").

-ifdef (LOCAL_MODE).
-define(MAKE_FILE,'LocalMakefile').
-else.
-define(MAKE_FILE,'Emakefile').
-endif.
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		make/1
		,update/0
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

make(Options) ->
	{ok,Emake} = file:consult(?MAKE_FILE),
	Opts = [{emake,Emake}| Options],
	make:all(Opts).

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
			io:format("ERROR:~n~p~n",[erlang:get_backtrace()]),
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