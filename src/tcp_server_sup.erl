-module(tcp_server_sup).

-behaviour(supervisor).

-export([start_child/1, start_link/2, init/1]).

-define(SERVER, ?MODULE).
-define(CLIENT_SUP, tcp_client_sup).
-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%%
%% @doc 创建客户端子进程，被tcp_client_sup监控
%%
start_child(Socket) ->
    supervisor:start_child(?CLIENT_SUP, [Socket]).

start_link(ListenPort, HandleMoudle) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [ListenPort, HandleMoudle]).

init([Port, Module]) ->
	TcpListener = {tcp_server_sup,                         		% Id       = internal id
                  {tcp_listener, start_link, [Port, Module]},	% StartFun = {M, F, A}
                  permanent,                               		% Restart  = permanent | transient | temporary
                  2000,                                    		% Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  		% Type     = worker | supervisor
                  [tcp_listener]                           		% Modules  = [Module] | dynamic
              },
	TcpClientSupervisor = {?CLIENT_SUP,
                  {supervisor, start_link, [{local, ?CLIENT_SUP}, ?MODULE, [Module]]},
                  permanent,
                  infinity,
                  supervisor,
                  []
              },
	{ok,
        {{one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [TcpListener, TcpClientSupervisor]
        }
    };

%%
%% @doc supervisor启动tcp_client_sup时的init
%%
init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module, start_link, []},                % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.