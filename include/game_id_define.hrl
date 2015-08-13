-define(HIGHGUID_TYPE_MASK 		,	16#FFF00000).
-define(HIGHGUID_TYPE_UNIT 		,	16#00000000).		%%npc,monster
-define(HIGHGUID_TYPE_ROLE 		,	16#10000000).		%%roleid
-define(HIGHGUID_TYPE_ITEM 		,	16#20000000).		%%itemid
-define(HIGHGUID_TYPE_PET  		,	16#30000000).		%%petid
-define(HIGHGUID_TYPE_LOOT 		,	16#40000000).		%%lootid
-define(HIGHGUID_TYPE_TRANSPORT	,	16#50000000).		%%transport
-define(HIGHGUID_TYPE_GUILD		,	16#60000000).		%%guildid
-define(HIGHGUID_TYPE_MAIL 		,	16#70000000).		%%mailid
-define(HIGHGUID_TYPE_AGENT 	,	16#80000000).		%%agentid

-define(HIGHGUID_TYPE_NOTICE 	,	16#11000000).		%%noticeid
-define(HIGHGUID_TYPE_VISITOR 	,	16#12000000).		%%visitorid


%%MaskDefined
-define(HIGHNPCID_DYNAMIC_MASK 	,	16#000F0000).		%%dynamic npcid
-define(HIGHGUID_PLATFORM_MASK 	,	16#000FF000).		%%platformid
-define(HIGHGUID_SERVER_MASK 	,	16#00000FFF).		%%serverid
-define(HIGHGUID_PLATSERVER_MASK 	,	16#000FFFFF). %%platform+server id


-define(GET_HIGHGUID(Id),(Id bsr 32)).
-define(GET_LOWGUID(Id),(Id band 16#FFFFFFFF)).


-define(CALC_PLATSERVER_ID(RoleId),(?GET_HIGHGUID(RoleId) band ?HIGHGUID_PLATSERVER_MASK)).
-define(CALC_PLATID_FROM_SERVERID(ServerId),(ServerId band ?HIGHGUID_PLATFORM_MASK) bsr 12).
-define(CALC_ONLYINDEXID_FROM_SERVERID(RoleId),(?GET_HIGHGUID(RoleId) band ?HIGHGUID_SERVER_MASK)).


-define(CALC_MINID_BY_TYPE_PLATESERVERID(TypeHigh,PLATSERVER),(TypeHigh bor PLATSERVER) bsl 32).
-define(CALC_FULLID_BY_TYPE_LOWER(High,Lower),((High bsl 32) bor Lower)  ).

-define(GET_ID_TYPE(Id),(?GET_HIGHGUID(Id) band ?HIGHGUID_TYPE_MASK )).

-define(IS_DYNAMIC_NPCID(Id),(((?GET_HIGHGUID(Id) band ?HIGHNPCID_DYNAMIC_MASK)=/=0) andalso ?GET_ID_TYPE(Id)=:=?HIGHGUID_TYPE_UNIT) ).
-define(CALC_DYNAMIC_MINID(),((?HIGHGUID_TYPE_UNIT bor 16#00010000) bsl 32)).