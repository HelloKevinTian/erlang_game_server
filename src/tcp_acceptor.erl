-module(tcp_acceptor).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

-export([test/0]).

-record(state, {
            listen_socket,  % 监听的端口
            ref
            }).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(LSocket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {LSocket}, []).

%% @doc just for test
test() ->
	gen_server:cast(?SERVER, 'test').

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({LSocket}) ->
    io:format("tcp_acceptor init ok.."),
    gen_server:cast(?SERVER, accept),
    {ok, #state{listen_socket = LSocket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(accept, #state{listen_socket = LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} ->
            {noreply, #state{listen_socket = LSock,ref = Ref}};
        Error ->
            {stop, {cannot_accept, Error}, #state{listen_socket = LSock}}
    end;

handle_cast(_Msg, State) ->
	io:format("Test tcp_acceptor ok! ~n"),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

