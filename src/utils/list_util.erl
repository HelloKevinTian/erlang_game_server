-include("base_define.hrl").

-module(list_util).

-compile(export_all).

combin_to_tuple_list(_,[])->
	[];
combin_to_tuple_list([],_)->
	[];
combin_to_tuple_list([V1|V1Lefts],[V2|V2Lefts])->
	[{V1,V2}|combin_to_tuple_list(V1Lefts,V2Lefts)].	


get_section(List1, List2) ->
	Is_section = fun(X) ->
				     lists:member(X, List2)
		     end,
	[X|| X <- List1, Is_section(X)].

get_difference(List1, List2) ->
	Section = get_section(List1, List2),
	Difference = fun(X) ->
				     not lists:member(X, Section)
		     end,
	Dif1 = [X|| X <- List1, Difference(X)],
	Dif2 = [X|| X <- List2, Difference(X)],
	Dif1 ++ Dif2.

delete([X|T], X) ->
	T;
delete([H|T], X) ->
	[H|delete(T, X)];
delete([], _X) ->
	[].

trunk([X|T], X) ->
	T;
trunk([_H|T], X) ->
	trunk(T, X);
trunk([], _X) ->
	[].


%%保持原序去重,性能差,不适用于长列表
unduplicate(List)->
	unduplicate_fun(List,[]).
unduplicate_fun([],ReList)->
	ReList;
unduplicate_fun([A|T],ReList)->
	?IF(lists:member(A,ReList),unduplicate_fun(T,ReList),unduplicate_fun(T,ReList++[A])).

%% F = fun(List)
foreach_step(StepN,F,L) when is_integer(StepN) , is_function(F,1) , is_list(L)->
	LSize = erlang:length(L),
	if LSize >StepN->
		   {L1,L2} = lists:split(StepN, L),
		   F(L1),
		   foreach_step(StepN,F,L2);
	   true->
		   F(L)
	end;
foreach_step(_,_,_)->
	nothing.


is_part_of(_,[])->
	false;
is_part_of(List,[_|T]=WholeList)->
	case lists:prefix(List,WholeList) of
		true->
			true;
		false->
			is_part_of(List,T)
	end.
	
replace_list(OriList,ListReplaced,ListReplace)->
	replace_list(lists:reverse(OriList),lists:reverse(ListReplaced),lists:reverse(ListReplace),[]).
replace_list([],_,_,Result)->
	Result;
replace_list([C|T]=NowList,ListReplaced,ListReplace,ResultTmp)->
	case lists:prefix(ListReplaced,NowList) of
		true->
			replace_list(NowList -- ListReplaced,ListReplaced,ListReplace,lists:reverse(ListReplace)++ResultTmp);
		false->
			replace_list(T,ListReplaced,ListReplace,[C|ResultTmp])
	end.



%% 根据NewValue查找List中第一个匹配Value，替换成NewValue，并返回新的列表
%% 返回:新列表
%% return：NewList
replace(List,Value,NewValue) ->
	replace([], List, Value,NewValue).
replace(HL, [], _Value,_NewValue) ->
    HL;
replace(HL,[Value|T], Value,NewValue) ->
	HL ++ [NewValue] ++ T;
replace(HL,[H|T], Value,NewValue) ->
	replace(HL ++ [H], T, Value,NewValue).


%% 重新整理list，从左至右遇到第一个Value，将其放置list尾部。
%% 返回:新列表
%% return：NewList
resort(List,Value) ->
	resort2([],List,Value).

resort2(HL, [],_Value) ->
    HL;
resort2(HL,[H|T],Value) ->
    case H of
		Value -> HL ++ T ++ [Value];
		_ -> resort2(HL ++ [H], T, Value)
    end.


%% 按Pos位置（从1开始）替换成NewValue，将NewValue放置在列表尾部，并将被替换的值返回
%% 返回:{新列表，被替换的值}
%% return：{NewL, undefined}/{NewList,ReplaceValue}
replace_by_pos(List, Pos, NewValue)->
	replace_by_pos2([], List, Pos, NewValue, 1).

replace_by_pos2(NewL, [], _Pos, _NewValue, _Index) ->
    {NewL, ?ERLNULL};
replace_by_pos2(NewL, [H|T], Pos, NewValue, Index) ->
    case Index of
	Pos ->
	    {NewL ++ T ++ [NewValue], H};
	_ ->
	    replace_by_pos2(NewL ++ [H], T, Pos, NewValue, Index + 1)
    end.


%% 按Pos位置（从1开始）替换成NewValue，并将被替换的值返回
%% 返回:{新列表，被替换的值}
%% return：{NewL, undefined}/{NewList,ReplaceValue}
replace_by_pos3(List, Pos, NewValue) ->
	replace_by_pos4([], List, Pos, NewValue, 1).

replace_by_pos4(NewL, [], _Pos, _NewValue, _Index) ->
	{NewL, ?ERLNULL};
replace_by_pos4(NewL, [H|T], Pos, NewValue, Index) ->
	case Index of
		Pos ->
			{NewL ++ [NewValue] ++ T, H};
		_ ->
			replace_by_pos4(NewL ++ [H], T, Pos, NewValue, Index + 1)
	end.
		  
		  
term_to_record_for_list([],_TableName) ->
	[];
term_to_record_for_list(Term,TableName) when is_list(Term) ->
	[list_to_tuple([TableName | tuple_to_list(Tup)]) ||Tup <- Term].	
 
%%return:0/Pos in List
get_pos_in_list(Elem,List)->
	get_pos_in_list(Elem,List,1).

get_pos_in_list(_Elem,[],_Index)->
	0;
get_pos_in_list(Elem,[Elem|_],Index)->
	Index;
get_pos_in_list(Elem,[_|Left],Index)->
	get_pos_in_list(Elem,Left,Index+1).

%% ------------------------------------------------------------------------
%% Function: get_random_list_with_count/2
%% Description: 从指定的列表中取出指定个数的随机列表
%% Input:
%%		Count: 指定个数
%%		List: 指定列表
%% Return: 随机列表
%% ------------------------------------------------------------------------
get_random_list_with_count(Count, List) when erlang:length(List) =< Count ->
	List;
get_random_list_with_count(Count, List) ->
	get_random_list_with_count(Count, List, []).

get_random_list_with_count(0, _List, Acc) ->
	Acc;
get_random_list_with_count(Count, List, Acc) ->
	Element = lists:nth(random:uniform(erlang:length(List)), List),
	get_random_list_with_count(Count-1, lists:delete(Element, List), [Element|Acc]).
