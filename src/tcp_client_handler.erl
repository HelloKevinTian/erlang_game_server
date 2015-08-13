-module(tcp_client_handler).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, addr}).
-define(TIMEOUT, 120000).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Socket]) ->
    logger:msg("New client process created with Pid: ~p~n",[self()]),
	inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {ok, #state{socket=Socket, addr=IP}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    inet:setopts(Socket, [{active, once}]),
	logger:msg("I(~p) Got message ~p from socket(~p)~n", [self(), Data, Socket]),
    ok = gen_tcp:send(Socket, <<"Echo back : ", Data/binary>>),
    {noreply, State};

handle_info({tcp_closed, Socket}, #state{addr=Addr} = State) ->
    logger:msg("Pid: ~p Socket: ~p Addr: ~p disconnected.~n", [self(), Socket, Addr]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------