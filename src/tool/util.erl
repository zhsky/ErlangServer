%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-10-22 10:48:27
%% @Doc:	DESC
%% @Last:	2019-10-22 12:51:17
%% ====================================================================

-module(util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		unixtime/0
		,term_to_binary/1
		,term_to_string/1
	]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 时间戳, 单位秒
unixtime() ->
	{M,S,_MM} = os:timestamp(),
	M * 1000000 + S.

term_to_binary(Term) when is_binary(Term) -> Term;
term_to_binary(Term) when is_atom(Term) ->
	list_to_binary(atom_to_list(Term));
term_to_binary(Term) when is_list(Term) ->
	list_to_binary(Term);
term_to_binary(Term) when is_integer(Term) ->
	list_to_binary(integer_to_list(Term));
term_to_binary(Term) when is_float(Term) ->
	Term1 = io_lib:format("~.2f", [Term]),
	list_to_binary(Term1);
term_to_binary(Term) when is_tuple(Term) ->
	list_to_binary(tuple_to_list(Term));
term_to_binary(_Msg) -> throw(other_value).

term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w",[Term]))).