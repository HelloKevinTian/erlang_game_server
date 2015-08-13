-module(run_option).
-compile(export_all).

get_options(File)->
	case file:consult(File) of
		{error,_Reason}->
			[];
		{ok,[RunOptions]}->
			RunOptions
	end.

get_opt_nodes(Options)->
	case lists:keyfind(nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}->NodesOption
	end.

get_opt_control_nodes(Options)->
	case lists:keyfind(control_nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}->NodesOption
	end.

get_opt_cache_nodes(Options)->
	case lists:keyfind(cache_nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}->NodesOption
	end.

get_opt_robot_nodes(Options)->
	case lists:keyfind(robot_nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}->NodesOption
	end.

get_opt_import_nodes(Options)->
	case lists:keyfind(import_nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}->NodesOption
	end.

get_opt_gen_nodes(Options)->
	case lists:keyfind(gen_nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}->NodesOption
	end.

get_opt_tool_nodes(Options)->
	case lists:keyfind(tool_nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}->NodesOption
	end.
get_opt_common(Options)->
	case lists:keyfind(common_option, 1, Options) of
		false-> undefined;
		{_,ComOpt}->ComOpt
	end.

get_opt_beam_dir(Options)->
	case lists:keyfind(beam_dir, 1, Options) of
		false-> "";
		{_,BeamDir}->BeamDir
	end.

get_db_dir(Options)->
		case lists:keyfind(nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}-> 
			Nodes = lists:filter(fun(NodeOpt)->
									{_,_,DBDir,_,_}=NodeOpt,
									DBDir=/=[]
								end,NodesOption),
			case Nodes of
				[]-> [];
				[{_,_,DBDir,_,_}|_]-> DBDir
			end
	end.
get_opt_prefix(Options)->
	case lists:keyfind(prefix, 1, Options) of
		false-> "";
		{_,Prefix}->Prefix
	end.

get_ip_list(Options)->
	case lists:keyfind(nodes, 1, Options) of
		false-> [];
		{_,NodesOption}->
			lists:map(fun(NodeOpt)-> 
						{_,Ip,_,_,_} = NodeOpt,
						Ip
						end, NodesOption)
	end.

get_useable_ips(Options)->
	IpList = run_option:get_ip_list(Options),
	MyIpList = os_util:get_localips(),
	lists:filter(fun(IpStr)->
						 lists:member(IpStr,IpList)
				 end,MyIpList).


get_node_ipstr(NodeName,Options)->
	NodesList = get_opt_nodes(Options),	
	case lists:keyfind(NodeName,1,NodesList) of
		false->
			[];
		{_,Ip,_,_,_}->
			Ip
	end.

get_servermgr(Options)->
	case lists:keyfind(servermanager,1,Options) of
		false->
			[];
		{_,NodeOption}->
			NodeOption
	end.

get_servershare_mgr(Options)->
	case lists:keyfind(server_share_mgr,1,Options) of
		false->
			[];
		{_,NodeOption}->
			NodeOption
	end.

get_httpproxy(Options)->
	case lists:keyfind(httpproxy,1,Options) of
		false->
			[];
		{_,NodeOption}->
			NodeOption
	end.

get_serverid(Options)->
	case lists:keyfind(serverid,1,Options) of
		false->
			0;
		{_,ServerId}->
			ServerId
	end.
	
get_node_name(Options,Node)->
	case lists:keyfind(nodes, 1, Options) of
		false-> undefined;
		{_,NodesOption}->
			case lists:keyfind(Node,1,NodesOption) of
				false->
					undefined;
				{NodeName,Ip,_,_,_}->
					ServerId = get_serverid(Options),
					list_to_atom("qq_s" ++ integer_to_list(ServerId) ++ "_" ++ NodeName ++ "@" ++ Ip)
			end
	end.

get_share_map_server(Options)->
	case lists:keyfind(share_map_server,1,Options) of
		false->
			false;
		{_,Value}->
			Value =:= 1
	end.	