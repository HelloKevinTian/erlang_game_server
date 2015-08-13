-module(gs_rpc).

-export([cast/2,cast/3,mult_cast/3]).

cast(NamedProc,Msg)->
	try
		NamedProc ! Msg
	catch
		_E:R->
			io:format("gs_rpc cast NamedProc ~p Msg ~p Reason ~p ERROR ~p ~n",[NamedProc,Msg,R,erlang:get_stacktrace()]),
			error
	end.


cast(Node,NamedProc,Msg)->
	CurNode = node(),
	try
		case Node of
			CurNode -> NamedProc!Msg;
			_Node  ->  {NamedProc,Node}!Msg%%rpc:abcast([Node],NamedProc, Msg) %% abcast 's first arg is NodeList
		end		
	catch 
		E:R ->
			io:format("gs_rpc:cast exception[~p:~p]!Node ~p NamedProc ~p Message ~p ~n~p ~n",
				[E,R,Node,NamedProc,Msg,erlang:get_stacktrace()]),
			error
	end.


mult_cast(Nodes,NamedProc,Msg) ->
	rpc:abcast(Nodes, NamedProc, Msg).