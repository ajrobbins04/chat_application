%%%-------------------------------------------------------------------
%% @doc chat_application public API
%% @end
%%%-------------------------------------------------------------------
-module(chat_application).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, []) ->
    {error, empty_start_args};
start(_StartType, [Port]) ->
    socket_sup:start_link([Port]).

stop(_State) ->
    ok.

%% internal functions