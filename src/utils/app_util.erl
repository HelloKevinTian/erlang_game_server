-module(app_util).

-export([get_argument/1]).

%% ---------------------------------------------------
%% erl -a b c -a d
%% 1> init:get_argument(a).
%% {ok,[["b","c"],["d"]]}
%% ---------------------------------------------------

get_argument(Input) when is_atom(Input)->
	case init:get_argument(Input) of
		error-> [];
		{ok, [ArgString]}-> lists:map(fun(E)-> list_to_atom(E) end, ArgString)
	end;

get_argument(Input) when is_list(Input)->
	case init:get_argument(list_to_atom(Input)) of
		error-> [];
		{ok, [ArgString]}-> lists:map(fun(E)-> list_to_atom(E) end, ArgString)
	end;

get_argument(_Input)->
	[].