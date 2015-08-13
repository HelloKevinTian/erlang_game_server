-module(logger).
-compile(export_all).

init(FileName)->
	% filelib:ensure_dir("../logs/"),
	error_logger:logfile({open, FileName}).

msg(Format, Data) ->
	% io:format(Format, Data),
	error_logger:info_msg(Format, Data).

msg(Format) ->
	% io:format(Format),
	error_logger:info_msg(Format).

error(Format)->
	% io:format(Format),
	error_logger:error_msg(Format).

error(Format, Data)->
	% io:format(Format,Data),
	error_logger:error_msg(Format,Data).

