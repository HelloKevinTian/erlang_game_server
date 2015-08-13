-define(ERLNULL,undefined).
-define(DATETIME_DEFALUT_VALUE,{{0,0,0},{0,0,0}}).
-define(ERL_NULL_TIMESTAMP,{0,0,0}).
-define(NUM_ZERO,0).
%% ?:
-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).
-define(IFDO(C, T),(case (C) of true -> (T); false -> nothing end)).