%% ====================================================================
%% @Author:	Payton
%% @Date:	2019-09-02 15:31:21
%% @Doc:	log
%% @Last:	2019-09-04 22:23:48
%% ====================================================================

-define(DEBUG(Format),log:debug_msg(?MODULE,?LINE,Format,[])).
-define(DEBUG(Format,Arge),log:debug_msg(?MODULE,?LINE,Format,Arge)).

-define(INFO(Format),log:info_msg(?MODULE,?LINE,Format,[])).
-define(INFO(Format,Arge),log:info_msg(?MODULE,?LINE,Format,Arge)).

-define(WARNING(Format),log:warning_msg(?MODULE,?LINE,Format,[])).
-define(WARNING(Format,Arge),log:warning_msg(?MODULE,?LINE,Format,Arge)).

-define(ERROR(Format),log:error_msg(?MODULE,?LINE,Format,[])).
-define(ERROR(Format,Arge),log:error_msg(?MODULE,?LINE,Format,Arge)).

-define(DTRACE(Format), log:debug_msg(?MODULE,?LINE,Format ++ ",~n~p~n", [try throw(trace) catch _:trace:Trace -> Trace end])).
-define(DTRACE(Format, Args), log:debug_msg(?MODULE,?LINE,Format ++ ",~n~p~n", Args ++ [try throw(trace) catch _:trace:Trace -> Trace end])).
