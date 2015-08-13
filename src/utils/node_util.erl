-module(node_util).

%%
%% Include files
%%
-include("base_define.hrl").
%%
%% Exported Functions
%%
-export([get_appnodes/1,
		 get_all_nodes/0,
		 get_all_nodes_for_global/0,
		 check_snode_match/2,
		 get_match_snode/2,
		 get_gatenodes/0,
		 get_mapnodes/0,
		 get_cachenodes/0,
		 get_robotnodes/0,
		 get_linenodes/0,
		 get_pathfindnodes/0,
		 get_dbnodes/0,
		 get_gmnodes/0,
		 get_timernodes/0,
		 get_crossnode/0,
		 get_controlnodes/0,
		 check_match_map_and_line/2,
		 get_dbnode/0,
		 get_gmnode/0,
		 get_robotnode/0,
		 get_mapnode/0,
		 get_cachenode/0,
		 get_timernode/0,
		 get_controlnode/0,
		 get_node_sname/1,
		 get_node_sname_str/1,
		 get_node_host/1,
		 get_map_node_index/1,
		 get_gate_node_index/1,
		 get_run_apps/1,
		 is_share_server_node/1,
		 get_nodes_without_hidden/0,
		 get_linenode/0
		 ]).
%%
%% API Functions
%%
get_appnodes(AppType)->
	system_option:get_node_option(AppType).

get_all_nodes()->
	case server_travels_util:is_share_server() of
		false->
			get_all_local_nodes();
		_->
			[node()| nodes()] ++ nodes(hidden)
	end.

get_all_local_nodes()->
	env:get(pre_connect_nodes, []).

get_nodes_without_hidden()->
	[node()| nodes()].

get_all_nodes_for_global()->
	case server_travels_util:is_share_server() of
		false->
			get_all_nodes();
		_->		%%global not reg not self node!
			lists:filter(fun(Node)-> node_util:is_share_server_node(Node) end, get_all_nodes())
	end.

check_snode_match(AppType,SNode)->
	SNodeStr = atom_to_list(SNode),
	lists:foldl(fun(Node,Acc)->
						AppNodeStr = atom_to_list(Node),
						case Acc of
							true-> true;
							_-> Index = string:str(SNodeStr, AppNodeStr),
								if Index>= 1 -> true;
								   true-> false
								end
						end
				end, false, get_appnodes(AppType)).
	
%	lists:member(get_node_sname(SNode), get_appnodes(AppType)).

is_share_server_node(SNode)->
	SNodeStr = get_match_snode_str(SNode),
	list_util:is_part_of("share",SNodeStr).

get_match_snode(AppType,SNode)->
	SNodeStr = get_match_snode_str(SNode),
	Ret = lists:foldl(fun(Node,Acc)->
						AppNodeStr = atom_to_list(Node),
						{_,TempRe} = Acc,
						case TempRe of
							true-> Acc;
							_-> 
								AppNodeStrLen = string:len(AppNodeStr),
								SNodeStrLen = string:len(SNodeStr),
								if
									AppNodeStrLen > SNodeStrLen->
										Acc;
									true->
										NewStr = string:substr(SNodeStr,SNodeStrLen-AppNodeStrLen+1), 
										if
											NewStr =:= AppNodeStr ->
												{Node,true};
											true->
												Acc
										end
								end
						end
				end, {SNode,false}, get_appnodes(AppType)),
	{MatchNode,_} = Ret,
	MatchNode.

get_gatenodes()->
	lists:filter(fun(Node)->
						 check_snode_match(gate,Node)
				 end, get_all_nodes()).	


get_mapnodes()->
	lists:filter(fun(Node)->
						 check_snode_match(map,Node)
				 end, get_all_nodes()).

get_robotnodes()->
	lists:filter(fun(Node)->
						 check_snode_match(robot,Node)
				 end, get_all_nodes()).

get_cachenodes()->
	lists:filter(fun(Node)->
						 check_snode_match(cache,Node)
				 end, get_all_nodes()).
				 
%% lineid 1 can macth mapnode xxxxmap1@ip
%% cannot macth mapnode xxxxmap10@ip
check_match_map_and_line(MapNode,LineId)->
	MapStr = "map"++erlang:integer_to_list(LineId),
	SNodeStr = get_match_snode_str(MapNode),		%%get string before @
	string:rstr(SNodeStr, MapStr) == length(SNodeStr) - length(MapStr) + 1 . %%mapstr is the end of snodestr

get_linenodes()->
	lists:filter(fun(Node)->
						 check_snode_match(line,Node)
				 end, get_all_nodes()).	

get_pathfindnodes()->
	lists:filter(fun(Node)->
						 check_snode_match(pathfind,Node)
				 end, get_all_nodes()).	

get_dbnodes()->
	lists:filter(fun(Node)->
						 check_snode_match(db,Node)
				 end, get_all_nodes()).	

get_gmnodes()->
	lists:filter(fun(Node)->
						 check_snode_match(gm,Node)
				 end, get_all_nodes()).	
	
get_timernodes()->
	lists:filter(fun(Node)->
						 check_snode_match(timer,Node)
				 end, get_all_nodes()).

get_crossnodes()->
	lists:filter(fun(Node)->
						 check_snode_match(cross,Node)
				 end, get_all_nodes()).	

get_crossnode()->
	case get_crossnodes() of
		[]-> ?ERLNULL;
		[Node|_]-> Node
	end.

get_dbnode()->
	global_node:get_global_proc_node(db_node).

get_gmnode()->
	global_node:get_global_proc_node(gm_node).

get_mapnode() ->
	case get_mapnodes() of
		[] -> ?ERLNULL;
		[Node|_] -> Node
	end.

get_robotnode() ->
	case get_robotnodes() of
		[] -> ?ERLNULL;
		[Node|_] -> Node
	end.

get_cachenode() ->
	case get_cachenodes() of
		[] -> ?ERLNULL;
		[Node|_] -> Node
	end.

get_timernode()->
	case get_timernodes() of
		[]-> ?ERLNULL;
		[Node|_]-> Node
	end.

get_controlnodes()->
	lists:filter(fun(Node)->
						 check_snode_match(control,Node)
				 end, get_all_nodes()).

get_controlnode()->
	case get_controlnodes() of
		[]->
			undefined;
		[Node|_]->
			Node
	end.

get_linenode() ->
	case get_linenodes() of
		[]-> undefined;
		[Node|_]-> Node
	end.

get_node_sname(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[NodeName,_Host]-> list_to_atom(NodeName);
		_-> ?ERLNULL
	end.
get_node_sname_str(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[NodeName,_Host]-> NodeName;
		_-> []
	end.
get_node_host(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[_NodeName,Host]-> Host;
		_-> []
	end.
get_match_snode_str(Node)->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[NodeName,_Host]-> NodeName;
		[NodeName]-> NodeName;
		_->[]
	end.

get_map_node_index(Node)->
	StrNode = atom_to_list(Node),
	[NodeName,_Host] = string:tokens(StrNode, "@"),
	Index = string:str(NodeName,"map"),
	try
		erlang:list_to_integer(string:substr(NodeName,Index+3))
	catch
		_E:_R->
			0
	end.

get_gate_node_index(Node)->
	StrNode = atom_to_list(Node),
	[NodeName,_Host] = string:tokens(StrNode, "@"),
	Index = string:str(NodeName,"gate"),
	try
		erlang:list_to_integer(string:substr(NodeName,Index+4))
	catch
		_E:_R->
			0
	end.

get_run_apps(Node)->
	SNode = get_node_sname(Node),
	NodeInfos = system_option:get_nodes_option(),
	FilterApp = fun({_App,Nodes})->
				  RealSNode = get_match_snode(_App,SNode),
				  lists:member(RealSNode, Nodes)
			 end,
	Apps = lists:filter(FilterApp, NodeInfos),
	lists:map(fun({App,_})-> App end, Apps).