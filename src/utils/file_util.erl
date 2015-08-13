-module(file_util).

%%
%% Include files
%%
-include_lib("kernel/include/file.hrl").
%%
%% Exported Functions
%%
-compile(export_all).
%%
%% API Functions
%%

append_to_file(File,AppendAllLines)->
	case file:open(File,[append]) of
		{ok,F}->
			file:write(F,AppendAllLines),
			file:close(F);
		{error,_}-> io:format("open file error~n")
	end.

get_file_all_line(FileName)->
	case file:open(FileName,[read]) of
		{ok,F}->
			read_file_loop(F);
		{error,Reason}-> io:format("open file ~p error: ~p ~n",[FileName,Reason])
	end.

read_file_loop(F)->
	case file:read_line(F) of
		{ok,Data}->
			[Data|read_file_loop(F)];
		eof->
			file:close(F),
			[];
		Error->
			slogger:error("read error ~p ~n",[Error]),
			file:close(F),
			[]
	end.

sure_dir_str(Dir)->
	case lists:last(Dir) of
		$/ -> Dir;
		_  -> Dir ++ "/"
	end.

ensure_dir(Dir)->
	filelib:ensure_dir(sure_dir_str(Dir)).


get_dir_name(Dir,Default)->
	Files = filelib:wildcard(filename:absname_join(Dir, "*.name")),
	case Files of
		[]->Default;
		[File|_]-> Base = filename:basename(File,".name"),
				   display_filename(Base)
	end.

display_filename(FileName)->
	case lists:any(fun(C)-> C>=255 end, FileName) of
		true-> binary_to_list(unicode:characters_to_binary(FileName, utf8));
		_-> FileName
	end.

get_dir_mtime(Dir)->
	{ok,FileInfo} = file:read_file_info(Dir),
	str_util:datetime_to_string(FileInfo#file_info.mtime).

dir_today_modify(Dir)->
	{ok,FileInfo} = file:read_file_info(Dir),
	{Date,_} = FileInfo#file_info.mtime,
	Date =:= erlang:date().

clear_db()->
	MemFiles = filelib:wildcard("../ebin/Mnesia*/*"),
	DbFiles = filelib:wildcard("../dbfile/*"),
	 
	lists:foreach(fun(Filename)->
						  file:delete(Filename) end , MemFiles ++ DbFiles),
	
	MemDirs =  filelib:wildcard("../ebin/Mnesia*"),
	lists:foreach(fun(Dir)->
						  file:del_dir(Dir) end , MemDirs).

clear_db(DBDir,MemDir)->
	DBWild = sure_dir_str(DBDir) ++ "*",
	MemWild = sure_dir_str(MemDir) ++ "Mnesia*/*",

	MemFiles = filelib:wildcard(MemWild),
	DbFiles = filelib:wildcard(DBWild),
	 
	lists:foreach(fun(Filename)->
						  file:delete(Filename) end , MemFiles ++ DbFiles),
	
	MemDirs =  filelib:wildcard(sure_dir_str(MemDir)++"Mnesia*"),
	lists:foreach(fun(Dir)->
						  file:del_dir(Dir) end , MemDirs).
remove_dir(Dir)->
	Cmd = "rm -rf " ++ Dir,
	os_util:wait_exe(Cmd, noprompt).


