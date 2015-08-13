# erlang_game_server
This is a game server written by Erlang.

version: 1.0

author: kevin

description:This is an erlang game server.


TCP部分测试代码:

1> {ok,S} = gen_tcp:connect({127,0,0,1},2222,[{packet,2}]).
	{ok,#Port<0.150>}

2> gen_tcp:send(S,<<"hello">>).
	ok

3> f(M), receive M -> M end.
	{tcp,#Port<0.150>,"hello"}


注意：
	1、app名称必须和node一致，否则generate报错