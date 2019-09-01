%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-01 15:33:08
%% @Doc:	根据mochiglobal.erl修改
%% @Last:	2019-09-01 20:09:42
%% ====================================================================

-module(compile_config).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	compile/2
	,get/3
	,get_list/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

compile(Mod,TermList) ->
	Bin = do_compile(Mod, TermList),
	code:purge(Mod),
	{module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
	ok.

get(Mod,Key,Default) ->
	try
		Mod:get(Key)
	catch
		_:_ -> Default
	end.

get_list(Mod) ->
	try
		Mod:get_list()
	catch
		_:_ -> []
	end.

do_compile(Module,TermList) ->
	Forms = [erl_syntax:revert(X) || X <- term_to_abstract(Module, TermList)],
	{ok, Module, Bin} = compile:forms(Forms),
	Bin.

term_to_abstract(Module,[]) ->
	[	%% -module(Module).
		erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(Module)])
	]
	++
	[	%% -export([get_list/0]).
		erl_syntax:attribute(
			erl_syntax:atom(export),
			[erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(get_list),erl_syntax:integer(0))])]
		)
	]
	++
	[	%% get_list() -> [].
		erl_syntax:function(
			erl_syntax:atom(get_list),
			[erl_syntax:clause([],none,[erl_syntax:abstract([])])]
		)
	];

term_to_abstract(Module,TermList) ->	
	[	%% -module(Module).
		erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(Module)])
	]
	++
	[	%% -export([get_list/0,get/1]).
		erl_syntax:attribute(
			erl_syntax:atom(export),
			[erl_syntax:list([
				erl_syntax:arity_qualifier(erl_syntax:atom(get_list),erl_syntax:integer(0)),
				erl_syntax:arity_qualifier(erl_syntax:atom(get),erl_syntax:integer(1))
			])]
		)
	]
	++
	[	%% get_list() -> [].
		erl_syntax:function(
			erl_syntax:atom(get_list),
			[erl_syntax:clause([],none,[erl_syntax:abstract(TermList)])]
		)
	]
	++
	[	%% get(K) -> V.
		erl_syntax:function(
			erl_syntax:atom(get),
			[
			begin
				case Term of
					{Key,Value} -> ok;
					_ ->
						Key = syntax_error,
						Value = Term
				end,
				erl_syntax:clause([erl_syntax:abstract(Key)], none, [erl_syntax:abstract(Value)])
			end
			|| Term <- TermList]
		)
	].
