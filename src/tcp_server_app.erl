-module(tcp_server_app).

-behaviour(application).

-export([start_client/1]).
-export([start/2, stop/1]).

-define(DEF_PORT,    2222).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client(Socket) ->
    tcp_server_sup:start_child(Socket).

start(_Type, _Args) ->
	do_init(),
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    tcp_server_sup:start_link(ListenPort, tcp_client_handler).

stop(_S) ->
    ok.

get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
	    {ok, Val} -> Val;
	    _ ->
	        case init:get_argument(Opt) of
		        [[Val | _]] -> Val;
		        error       -> Default
	        end
    end.

do_init() ->
	logger:init("../../logs/server.log"),
	test_redis().
	% test_proto().

test_redis() ->
	{ok, C} = eredis:start_link(),
	{ok, Values} = eredis:q(C, ["keys", "*"]),
	logger:msg("Some message with a term: ~n~p~n", [Values]).

% test_proto() ->
% 	protobuffs_compile:scan_file("simple.proto"),
% 	Val = simple_pb:decode_person(<<10,4,78,105,99,107,18,13,77,111>>),
% 	logger:msg("@@@@@@  ~p~n",[Val]).