%% tcp_server module handles the server-side logic of accepting 
%% incoming connections, managing client sessions, and facilitating
%% communication between clients and other server components.
-module(tcp_server).
-behavior(gen_server).

%% defines the state of a server process that 
%% listens for incoming connections on a port
-record(server_state, {
    listen_socket :: inet:socket(),  % used by the server for accepting incoming connections
    processor  % the function responsible for processing incoming messages
}).

%% gen_server behaviour uses the following 
%% callbacks: init/1, handle_call/3, handle_cast/2, 
%% handle_info/2, and terminate/2
-export([start_link/1, init/1]).


start_link(ListenSocket) ->
    % spawns new child process 
    gen_server:start_link({local, ?MODULE}, ?MODULE, ListenSocket, []).

% callback function to initialize the server (the 3rd arg of start_link/4)
init(ListenSocket) ->
    gen_server:cast(self(), {start, ListenSocket}),
	{ok, #server_state{listen_socket=ListenSocket}}.

%% handles async messages (called 'casts') that 
%% don't expect a response from the server state
handle_cast({start, ListenSocket}, State) ->
    accept_connections(ListenSocket),
    {noreply, State}. % tuples w/noreply are valid

%% handles synchronous requests
%%handle_call(_Request, _From, State) ->
    % typically sends {reply,Reply,NewState} tuple
   %% .

%% called when any "handle" function's return
%% tuple contains 'stop' as its reply
%%terminate(Reason, State) ->.


accept(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> client_handler:handle_client(AcceptSocket) end),
    accept(ListenSocket). % continue accepting incoming connections