-define(GRID_WIDTH, 1200).
-define(CREATURE_VIEW_RANGE,1200).
-define(GRID_WIDTH_INSTANCE, 1200).
-define(CREATURE_VIEW_RANGE_INSTANCE,1200).

-define(DICT_GRIDE_UPDATE_TIME,grid_update_time).
-define(DICT_GRIDE_IS_INSTANCE,grid_is_instance).

-define(PIXEL_ONE_CELL,16).

-define(CACL_AROUND_NINE_GRID(Index_x, Index_y),([{Index_x - 1, Index_y - 1},{Index_x - 1, Index_y},{Index_x - 1, Index_y + 1},
												  {Index_x, Index_y - 1},{Index_x, Index_y},{Index_x, Index_y + 1},
												  {Index_x + 1, Index_y - 1},{Index_x + 1, Index_y},{Index_x + 1, Index_y + 1}])).

-define(BOTTOM,1). 
-define(RIGHT_BOTTOM,2).
-define(RIGHT,3).
-define(RIGHT_TOP,4).
-define(TOP,5).
-define(LEFT_TOP,6).
-define(LEFT,7).
-define(LEFT_BOTTOM,8).
-define(CENTER,0).