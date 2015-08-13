-module(game_id_util).

-include("game_id_define.hrl").

-export([what_id/1,make_full_unitid_by_low/1,get_lowid/1,get_platserverid/1,
		 is_care_creature/1,make_full_lootid_by_low/1,make_full_transportid_by_low/1,
		 get_platid/1,get_server_indexid/1]).

what_id(Id)->
	case ?GET_ID_TYPE(Id) of
		?HIGHGUID_TYPE_UNIT->
			npc;
		?HIGHGUID_TYPE_ROLE->
			role;
		?HIGHGUID_TYPE_PET->
			pet;
		?HIGHGUID_TYPE_ITEM->
			item;
		?HIGHGUID_TYPE_LOOT->
			loot;
		?HIGHGUID_TYPE_TRANSPORT->
			transport;
		?HIGHGUID_TYPE_GUILD->
			guild;
		?HIGHGUID_TYPE_MAIL->
			mail;
		?HIGHGUID_TYPE_AGENT ->
			agent
	end.

is_care_creature(Id)->
	case what_id(Id) of
		npc->
			true;
		role->
			true;
		pet->
			true;
		agent ->
			true;
		_->
			false
	end.
		
make_full_unitid_by_low(LowerId)->
	?CALC_FULLID_BY_TYPE_LOWER(?HIGHGUID_TYPE_UNIT,LowerId).

make_full_lootid_by_low(LowerId)->
	?CALC_FULLID_BY_TYPE_LOWER(?HIGHGUID_TYPE_LOOT,LowerId).

make_full_transportid_by_low(LowerId)->
	?CALC_FULLID_BY_TYPE_LOWER(?HIGHGUID_TYPE_TRANSPORT,LowerId).

get_lowid(Id)->
	?GET_LOWGUID(Id).

get_platserverid(Id)->
	?CALC_PLATSERVER_ID(Id).

get_platid(ServerId)->
	?CALC_PLATID_FROM_SERVERID(ServerId).

get_server_indexid(Id)->
	?CALC_ONLYINDEXID_FROM_SERVERID(Id).