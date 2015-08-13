-module(math_util).

-export([even_div/2]).

%% 向上取整
even_div(Number,Divisor)->
	FloatNum = Number/Divisor,
	if
		 FloatNum - erlang:trunc(FloatNum)>0 ->
		 	erlang:trunc(FloatNum)+1;
		 true->	
		 	erlang:trunc(FloatNum)
	end.