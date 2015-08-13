-module(time_util).

-record(system_time, {year ,month ,day ,hour ,minute ,second}).

-export([
		 now_to_s/1,now_to_ms/1,get_left_time_to_next_time/2,
		 check_is_overdue/2,get_time_compare_trunc/1,
		 check_sec_is_in_timeline_by_day/2,check_dateline/1,check_dateline_by_range/1,
		 get_natural_days_from_now/1,
		 check_same_day/2,
		 check_same_week/2,
		 is_in_time_point/3,
		 is_datetime_over/2,
		 is_time_over/2,
		 time_diff/2,
		 day_diff/2,
		 s_to_now/1,
		 get_last_moment_in_curweek/0]).

-export([
		 diff_secs/2,
		 make_time/1,
		 make_time/6,
		 next_day_from_now/1
		]).
		 
time_diff(Time1,Time2)->
	calendar:time_to_seconds(Time1) - calendar:time_to_seconds(Time2).

day_diff(T1,T2)->
	{Date1,_} = calendar:now_to_local_time(T1),
	{Date2,_} = calendar:now_to_local_time(T2),
	calendar:date_to_gregorian_days(Date1) -  calendar:date_to_gregorian_days(Date2).
	
%% Now1>Now2 Now1要比Now2晚.	
check_same_week(Now1,Now2)->
	{Date1,_} = calendar:now_to_local_time(Now1),
	{Date2,_} = calendar:now_to_local_time(Now2),
	Days1 = calendar:date_to_gregorian_days(Date1),
	Days2 = calendar:date_to_gregorian_days(Date2),
	Week1 = calendar:day_of_the_week(Date1),
	Week2 = calendar:day_of_the_week(Date2),
	if
		Days1-Days2>=7->
			true;
		true->
			Week1<Week2	
	end.
	
get_left_time_to_next_time(NowTime,{Hour,Min,Sec})->
	{{_,_,_},{Hnow,Mnow,Snow}} = calendar:now_to_local_time(NowTime),
	NowTimes = calendar:datetime_to_gregorian_seconds({{1,1,1},{Hnow,Mnow,Snow}}),
	TodayOverTimes = calendar:datetime_to_gregorian_seconds({{1,1,1},{Hour,Min,Sec}}),
	if
		TodayOverTimes > NowTimes->		
		 	TodayOverTimes - NowTimes;
		true->							
			calendar:datetime_to_gregorian_seconds({{1,1,2},{Hour,Min,Sec}}) - NowTimes
	end.	
	
now_to_ms({A,B,C})->
	A*1000000000+B*1000 + C div 1000.
	
now_to_s({A,B,_})->
	A*1000000+B.
	
s_to_now(S)->
	{S div 1000000, S rem 1000000, 0}.
	
check_is_overdue({ClearH,ClearMin,ClearSec},FristTime)->
	NowTime = now(),
	NowDate = calendar:now_to_local_time(NowTime),
	FirstDate = calendar:now_to_local_time(FristTime),
	{{FirstY,FirstM,FirstD},{FirstH,FirstMin,FirstSec}} = FirstDate, 
	{{NowY ,NowM,NowD},{NowH,NowMin,NowSec}} = NowDate,
	DiffDays = calendar:date_to_gregorian_days(NowY ,NowM,NowD) - calendar:date_to_gregorian_days(FirstY,FirstM,FirstD),
	if
		(DiffDays > 1)->		%%相差日期超过一天
			true;
		true->
			FirstSecs = {{FirstY,FirstM,FirstD},{FirstH,FirstMin,FirstSec}},
			NowSecs = {{NowY ,NowM,NowD},{NowH,NowMin,NowSec}},
			(
			 (compare_datatime(FirstSecs,{{FirstY,FirstM,FirstD},{ClearH,ClearMin,ClearSec}})=:=true) and
				 (compare_datatime({{FirstY,FirstM,FirstD},{ClearH,ClearMin,ClearSec}},NowSecs)=:=true)
			)
				or
				(
				 (compare_datatime(FirstSecs,{{NowY ,NowM,NowD},{ClearH,ClearMin,ClearSec}})=:=true ) and
					 (compare_datatime({{NowY ,NowM,NowD},{ClearH,ClearMin,ClearSec}},NowSecs)=:=true )	 
				)
	end.	

check_sec_is_in_timeline_by_day(CheckTime,TimeLine) ->
	{{{_,_,_},StartHourMinSec},EndTime} = TimeLine,
	is_in_time_point({{0,0,0},StartHourMinSec},EndTime,CheckTime).
	
check_dateline(DateLines)->
	if
		DateLines =:= []->
			true;
		true->
			NowTime = calendar:now_to_local_time(os:timestamp()),
			check_dateline(NowTime,DateLines)
	end.
	
check_dateline(NowTime,DateLines)->	
	lists:foldl(fun({BeginTime,EndTime},Result)->
	if
		Result->
			true;
		true->
			is_in_time_point(BeginTime,EndTime,NowTime)
	end end,false,DateLines).	

check_dateline_by_range(DateLines)->
	if
		DateLines =:= []->
			true;
		true->
			NowTime = calendar:now_to_local_time(os:timestamp()),
			lists:foldl(fun({BeginTime,EndTime},Result)->
						if
							Result->
								true;
							true->
								is_in_time_point(BeginTime,EndTime,NowTime)
						end end,false,DateLines)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%								判断当前时间是否在两个时间点之间 is_in_time_point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%如果结束时间为{{0,0,0},{0,0,0}},约定为无结束时间,只判断是否到开启时间即可
is_in_time_point(StartTime,{{0,0,0},{0,0,0}},NowTime)->
	is_datetime_over(StartTime,NowTime);
%%如果开始时间为{{0,0,0},{0,0,0}},结束时间不为,只判断是否还未到期
is_in_time_point({{0,0,0},{0,0,0}},EndTime,NowTime)->
	is_datetime_over(NowTime,EndTime);
%%如果起始年月日为0,则只判断时分秒,默认为判断一天内的时间点判断.
is_in_time_point({{0,0,0},BeginHourMinSec}=_StartTime,{_,EndHourMinSec}=_EndTime,{_,NowHourMinSec}=_NowTime)->
	is_time_over(BeginHourMinSec,NowHourMinSec) and	is_time_over(NowHourMinSec,EndHourMinSec);
is_in_time_point(StartTime,EndTime,NowTime)->
	is_datetime_over(StartTime,NowTime) and is_datetime_over(NowTime,EndTime).

%%用于判断时间点2是否在时间点1之后
is_datetime_over(DateTime1,DateTime2)->
	compare_datatime(DateTime1,DateTime2) =/= false.
is_time_over(Time1,Time2)->
	compare_time(Time1,Time2) =/= false.

%%用于一个时间点与令一个时间点的先后比较		
%%返回true(Time2大于Time1)/false(Time2小于Time1)/equal(Time2等于Time1)
compare_datatime({YearMonthDay1,HourMinSec1},{YearMonthDay2,HourMinSec2})->
	case compare_time(YearMonthDay1,YearMonthDay2) of
		true->			
			true;
		equal->			%%两个时间点在同一天,检测时分秒
			compare_time(HourMinSec1,HourMinSec2);
		false->
			false
	end.	

%%用于年月日/时分秒的比较
%%返回true(Time2大于Time1)/false(Time2小于Time1)/equal(Time2等于Time1)
compare_time({A1,B1,C1},{A2,B2,C2})->
	if
		A2>A1->
			true;
		A2=:=A1->
			if
				B2>B1->
					true;
				B2=:=B1->
					if
						C2>C1->
							true;
						C2=:=C1->
							equal;
						true->
							false
					end;		
				true->
					false
			end;			
		true->
			false
	end.

get_time_compare_trunc({hour,CompareTime})->
	CheckTime = calendar:now_to_local_time(CompareTime),
	NowTime = calendar:now_to_local_time(now()),
	CheckSecs = calendar:datetime_to_gregorian_seconds(CheckTime),
	NowSecs = calendar:datetime_to_gregorian_seconds(NowTime),
	if
		NowSecs > CheckSecs->
			MiddleSecs = NowSecs - CheckSecs,
			erlang:trunc(MiddleSecs/3600);
		true->
			0
	end;

get_time_compare_trunc({min,CompareTime})->
	CheckTime = calendar:now_to_local_time(CompareTime),
	NowTime = calendar:now_to_local_time(now()),
	CheckSecs = calendar:datetime_to_gregorian_seconds(CheckTime),
	NowSecs = calendar:datetime_to_gregorian_seconds(NowTime),
	if
		NowSecs > CheckSecs->
			MiddleSecs = NowSecs - CheckSecs,
			erlang:trunc(MiddleSecs/60);
		true->
			0
	end;

get_time_compare_trunc({second,CompareTime})->
	CheckTime = calendar:now_to_local_time(CompareTime),
	NowTime = calendar:now_to_local_time(now()),
	CheckSecs = calendar:datetime_to_gregorian_seconds(CheckTime),
	NowSecs = calendar:datetime_to_gregorian_seconds(NowTime),
	if
		NowSecs > CheckSecs->
			MiddleSecs = NowSecs - CheckSecs,
			MiddleSecs;
		true->
			0
	end;

get_time_compare_trunc({day,CompareTime})->
	CheckTime = calendar:now_to_local_time(CompareTime),
	NowTime = calendar:now_to_local_time(now()),
	CheckSecs = calendar:datetime_to_gregorian_seconds(CheckTime),
	NowSecs = calendar:datetime_to_gregorian_seconds(NowTime),
	if
		NowSecs > CheckSecs->
			MiddleSecs = NowSecs - CheckSecs,
			erlang:trunc(MiddleSecs/86400);
		true->
			0
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 取得当前时间与传入参数时间的自然天间隔数，参数是Unix时间戳
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
get_natural_days_from_now(LastNow)->
	{NowDate,_NowTime} = calendar:now_to_local_time(now()),
	NowDays = calendar:date_to_gregorian_days(NowDate),
	{CheckDate,_CheckTime} = calendar:now_to_local_time(LastNow),
	CheckDays = calendar:date_to_gregorian_days(CheckDate),
	if
		NowDays > CheckDays->
			NowDays - CheckDays;
		true->
			0
	end.
%%
%% 
%%

%%
%%检测两个时间是否为同一天
%%
%% T1 = T2 = {X,Y,Z}
%% return true | false
check_same_day(T1,T2)->
	{Date1,_} = calendar:now_to_local_time(T1),
	{Date2,_} = calendar:now_to_local_time(T2),
	Date1 =:= Date2.

%% @spec 
%% @doc 获取本周周日23:59:00的时间戳
%% @end 
get_last_moment_in_curweek()->
	{Date,_} = calendar:local_time(),
	NowSec = calendar:datetime_to_gregorian_seconds({Date,{23,59,0}}),
	CurWeek = calendar:day_of_the_week(Date),
	LastSec = NowSec + (7 - CurWeek) * 3600 * 24,
	LastSec.

%% 两个时间的时间差 单位s  NewT1 > OldT2
diff_secs(NewT1={_,_,_},OldT2={_,_,_})->
	NewSecs = calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(NewT1)),
	OldSecs = calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(OldT2)),
	NewSecs - OldSecs;
diff_secs(NewDt1={_,_},OldDt2={_,_})->
	NewSecs = calendar:datetime_to_gregorian_seconds(NewDt1),
	OldSecs = calendar:datetime_to_gregorian_seconds(OldDt2),
	NewSecs - OldSecs.


make_time(Now = {_, _, _}) ->
	{{Y, M, D}, {HH, MM, SS}} = calendar:now_to_local_time(Now),
	make_time(Y, M, D, HH, MM, SS).
make_time(Y, M, D, HH, MM, SS) ->
	#system_time{year = Y, month = M, day = D, hour = HH, minute = MM, second = SS}.

next_day_from_now(Now) ->
	Sec = now_to_s(Now),
	Sec2 = Sec + 24*60*60,
	Now2 = s_to_now(Sec2),
	{NextDate, _} = calendar:now_to_local_time(Now2),
	NextDate.