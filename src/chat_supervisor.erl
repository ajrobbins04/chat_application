%%%-------------------------------------------------------------------
%% @doc chat_application top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(socket_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Port) ->
    ServerName = chat_server,
    % starts the gen_server supervisor at the given Port 
    supervisor:start_link({local, ServerName}, ?MODULE, [Port]).

init([Port]) ->
    TCP_OPTIONS = [{active, once}, {packet, line}], 
    % sets up listening socket at the given port location
    case start_listen_socket(Port, TCP_OPTIONS) of
        {ok, ListenSocket} ->
            start_tcp_servers(ListenSocket);
        {error, Reason} ->
            {error, Reason}
        end.

%% internal functions
start_listen_socket(Port, Options) ->
    case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            spawn_link(fun empty_listeners/0),
            {ok, ListenSocket};
        {error, Reason} ->
            {error, Reason}
    end.
    
%% create tcp_server worker processes using supervisor specs
start_tcp_servers(ListenSocket) ->
    SupSpecifications = {
        % failed worker process restarts automatically
        {simple_one_for_one, 60, 3600},
        [
            % worker processes will be tcp_servers, is is started using start_link/1 with a ListenSocket arg
            {tcp_server, {tcp_server, start_link, [ListenSocket]}, temporary, 1000, worker, [tcp_server]}
        ]
    },
    {ok, SupSpecifications}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

%% starts 20 empty listener processes 
%% to handle incoming connections
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.