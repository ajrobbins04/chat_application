%%%-------------------------------------------------------------------
%% @doc chat_application public API
%% @end
%%%-------------------------------------------------------------------
-module(chat_application).
-behaviour(application).

-export([start/0, start/1, stop/1]).

start() ->
    socket_sup:start_link(8091).
start(Port) ->
    socket_sup:start_link(Port).

stop(_State) ->
    ok.

%% internal functions