-module(protobuf_client).

-compile(export_all).


start() ->
	Message = <<"client 2 server test message. !@#$%^&*()_+">>,
    {ok,Sock} = gen_tcp:connect("127.0.0.1",2222,[{active,false}, {packet,2}]),
    gen_tcp:send(Sock,Message),
    A = gen_tcp:recv(Sock,0),
    gen_tcp:close(Sock),
    A.

