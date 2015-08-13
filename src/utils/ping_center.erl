-module(ping_center).

%%
%% Exported Functions
%%
-export([wait_all_nodes_connect/0,wait_all_nodes_connect/1,ping/1,wait_node_connect/1]).

%%
%% API Functions
%%
wait_all_nodes_connect()->
	AllNodes = env:get(pre_connect_nodes,[]),
	wait_nodes(AllNodes).

wait_all_nodes_connect(Flag)->
	AllNodes = env:get(pre_connect_nodes,[]),
	wait_nodes(AllNodes,Flag).

wait_node_connect(Type)->
	NeedConNodes = lists:filter(fun(Node)-> node_util:check_snode_match(Type,Node) end, env:get(pre_connect_nodes,[])),
	wait_nodes(NeedConNodes).
	
wait_nodes(AllNodes)->
	slogger:msg("need wait nodes ~p ~n",[AllNodes]),
	lists:foreach(fun(Node)-> 
		slogger:msg("ping Node ~p ~n",[Node]),				  
		ping(Node) end,AllNodes).	

wait_nodes(AllNodes,Flag)->
	slogger:msg("need wait nodes ~p ~n",[AllNodes]),
	lists:foreach(fun(Node)-> 
		slogger:msg("ping Node ~p Flag ~p ~n",[Node,Flag]),				  
		if
			Flag->
				case string:str(atom_to_list(Node), "control") of
					0->
						ping(Node);
					_->
						ping(Node,3)
				end;
			true->
				ping(Node,3)
		end
	end,AllNodes).	
	
ping(Node)->	
	ping_loop(Node).

ping_loop(Node)->
	case net_adm:ping(Node) of
		pong -> ok;
		_->
			receive 
			after
				1000 -> ping_loop(Node)
			end
	end.


ping(Node,Num)->
	ping_loop(Node,Num).

ping_loop(_Node,0)->
	ok;
ping_loop(Node,Num)->
	case net_adm:ping(Node) of
		pong -> ok;
		_->
			receive 
			after
					1000 -> ping_loop(Node,Num-1)
			end
	end.