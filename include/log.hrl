%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-02 15:31:21
%% @Doc:	log
%% @Last:	2019-09-02 17:14:31
%% ====================================================================

-define(DEBUG_MSG(Format),log:debug_msg(?MODULE,?LINE,Format,[])).
-define(DEBUG_MSG(Format,Arge),log:debug_msg(?MODULE,?LINE,Format,Arge)).

-define(INFO_MSG(Format),log:info_msg(?MODULE,?LINE,Format,[])).
-define(INFO_MSG(Format,Arge),log:info_msg(?MODULE,?LINE,Format,Arge)).

-define(WARNING_MSG(Format),log:warning_msg(?MODULE,?LINE,Format,[])).
-define(WARNING_MSG(Format,Arge),log:warning_msg(?MODULE,?LINE,Format,Arge)).

-define(ERROR_MSG(Format),log:error_msg(?MODULE,?LINE,Format,[])).
-define(ERROR_MSG(Format,Arge),log:error_msg(?MODULE,?LINE,Format,Arge)).

-define(DTRACE(Format), log:debug_msg(?MODULE,?LINE,Format ++ ",~n~p~n", [try throw(trace) catch trace -> erlang:get_stacktrace() end])).
-define(DTRACE(Format, Args), log:debug_msg(?MODULE,?LINE,Format ++ ",~n~p~n", Args ++ [try throw(trace) catch trace -> erlang:get_stacktrace() end])).
