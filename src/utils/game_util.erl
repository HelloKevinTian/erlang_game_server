-module(game_util).

-include("map_grid_define.hrl").

-export([
		 is_in_range/3,
		 get_straight_distance/2,
		 get_move_time_ms/3,
		 random_by_weighting/1,
		 get_random_list_from_list/2,
		 get_straight_cell_num/2,
		 get_jump_time_ms/3,
		 get_walk_orientation/2
		]).

is_in_range({Pos1x,Pos1y},{Pos2x,Pos2y},Range)->
	((erlang:abs(Pos1x - Pos2x) =< Range) and (erlang:abs(Pos1y - Pos2y) =< Range)).

get_straight_cell_num({Pos1x,Pos1y},{Pos2x,Pos2y})->
	InclinedNum = Pos1x - Pos2x,
	StraightNum = Pos1y - Pos2y,
	trunc(math:sqrt(InclinedNum*InclinedNum + StraightNum*StraightNum)).

get_walk_orientation({StartX,StartY},{EndX,EndY}) ->
	X = EndX - StartX,
	Y = EndY - StartY,
	if
		X=:=0 ->
			if 
				Y>0->
					?BOTTOM;
				Y<0->
					?TOP
			end;
		X>0->
			if 
				Y>0->
					?RIGHT_BOTTOM;
				Y<0->
					?RIGHT_TOP;
				true->
					?RIGHT
			end;
		X<0->
			if 
				Y>0->
					?LEFT_BOTTOM;
				Y<0->
					?LEFT_TOP;
				true->
					?LEFT
			end;
		true->
			?BOTTOM
	end.
	
get_straight_distance({X1,Y1},{X2,Y2})->
	InclinedNum = abs(X2 - X1)*?PIXEL_ONE_CELL,
	StraightNum = abs(Y2 - Y1)*?PIXEL_ONE_CELL,
	trunc(math:sqrt(InclinedNum*InclinedNum + StraightNum*StraightNum)).

get_move_time_ms({X1,Y1},{X2,Y2},PixelSpeed)->
	InclinedNum = min(abs(X2 - X1),abs(Y2-Y1)),
	StraightNum = max(abs(X2 - X1),abs(Y2-Y1)) - InclinedNum,
	trunc((StraightNum*1000 +  InclinedNum*1414)*?PIXEL_ONE_CELL/(PixelSpeed)).

get_jump_time_ms({X1,Y1},{X2,Y2},PixelSpeed)->
	XPixel = (X2 - X1),
	YPixel = (Y2 - Y1),
	trunc(math:sqrt(XPixel*XPixel + YPixel*YPixel)*?PIXEL_ONE_CELL*1000/(PixelSpeed*1.6)).

%%result: Term/[]
%%RandomList:[{Term,Weighting}]
random_by_weighting(RandomList)->
	FullWeight = lists:foldl(fun(ItemWight,LastWight)->
						LastWight +element(2,ItemWight)
				end, 0, RandomList),
	if
		FullWeight =< 0->
			[];
		true->
			RandomV = random:uniform(FullWeight),
			{Value,_} = lists:foldl(fun({Term,WightTmp},{LastValue,LastWight})->
						if
							LastValue =/= []->
								{LastValue,0};
							true->
								if
									LastWight+WightTmp >= RandomV->			%%binggo
										{Term,0};
									true->
										{[],LastWight+WightTmp}
								end
						end
				end, {[],0}, RandomList),
			Value
	end.

get_random_list_from_list([],_Count)->
	[];
get_random_list_from_list(List,Count)->
	get_random_list_from_list(Count,List,erlang:length(List),List).
get_random_list_from_list(0,_,_,_OriList)->
	[];								
get_random_list_from_list(Count,[],_,OriList)->
	get_random_list_from_list(OriList,Count);
get_random_list_from_list(Count,List,Len,OriList)->
	Random = random:uniform(Len),
	Tuple = lists:nth(Random, List),
	[Tuple|get_random_list_from_list(Count-1,lists:delete(Tuple, List),Len-1,OriList)].	
