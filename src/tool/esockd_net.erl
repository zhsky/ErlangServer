%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(esockd_net).

-include_lib("kernel/include/inet.hrl").

-export([hostname/1, hostname/0]).
-export([format_addr/2, format_addr/1]).
-export([format_ip/1]).

hostname({0,0,0,0}) ->
    hostname();
hostname({0,0,0,0,0,0,0,0}) ->
    hostname();
hostname(IPAddress) ->
    case inet:gethostbyaddr(IPAddress) of
        {ok, #hostent{h_name = Name}} -> Name;
        {error, _Reason} -> ntoa(IPAddress)
    end.
hostname() ->
    {ok, Hostname} = inet:gethostname(),
    case inet:gethostbyname(Hostname) of
        {ok,    #hostent{h_name = Name}} -> Name;
        {error, _Reason}                 -> Hostname
    end.

format_addr(sockname, Sockname) ->
    format_addr(Sockname);
format_addr(peername, Peername) ->
    format_addr(Peername).
format_addr({Addr, Port}) ->
    lists:flatten(io_lib:format("~s:~p", [format_ip(Addr), Port])).

format_ip(Addr) when is_tuple(Addr) -> ntoab(Addr);
format_ip(Host) -> Host.

ntoa({0,0,0,0,0,16#ffff,AB,CD}) ->
    inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256});
ntoa(IP) -> inet_parse:ntoa(IP).

ntoab(IP) ->
    Str = ntoa(IP),
    case string:str(Str, ":") of
        0 -> Str;
        _ -> "[" ++ Str ++ "]"
    end.

