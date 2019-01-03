pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with LV.Area;
with LV.Color;
with Interfaces.C.Extensions;
with LV.Style;

package LV.Objx.Chart is

   subtype Instance is Obj_T;

   type Series is private;

   type lv_chart_type_t is
     (TYPE_LINE,
      TYPE_COLUMN,
      TYPE_POINT)
     with Size => 8;

   for lv_chart_type_t use
     (TYPE_LINE   => 1,
      TYPE_COLUMN => 2,
      TYPE_POINT  => 4);

   function create (Par : Obj_T; Copy : Obj_T) return Instance;  -- lv_chart.h:80
   pragma Import (C, create, "lv_chart_create");

   function add_series (Self : Instance; arg2 : LV.Color.lv_color_t) return Series;  -- lv_chart.h:92
   pragma Import (C, add_series, "lv_chart_add_series");

   procedure set_div_line_count
     (Self : Instance;
      arg2 : uint8_t;
      arg3 : uint8_t);  -- lv_chart.h:104
   pragma Import (C, set_div_line_count, "lv_chart_set_div_line_count");

   procedure set_range
     (Self : Instance;
      arg2 : LV.Area.Coord_T;
      arg3 : LV.Area.Coord_T);  -- lv_chart.h:112
   pragma Import (C, set_range, "lv_chart_set_range");

   procedure set_type (Self : Instance; arg2 : lv_chart_type_t);  -- lv_chart.h:119
   pragma Import (C, set_type, "lv_chart_set_type");

   procedure set_point_count (Self : Instance; arg2 : uint16_t);  -- lv_chart.h:126
   pragma Import (C, set_point_count, "lv_chart_set_point_count");

   procedure set_series_opa (Self : Instance; arg2 : LV.Color.lv_opa_t);  -- lv_chart.h:133
   pragma Import (C, set_series_opa, "lv_chart_set_series_opa");

   procedure set_series_width (Self : Instance; arg2 : LV.Area.Coord_T);  -- lv_chart.h:140
   pragma Import (C, set_series_width, "lv_chart_set_series_width");

   procedure set_series_darking (Self : Instance; arg2 : LV.Color.lv_opa_t);  -- lv_chart.h:147
   pragma Import (C, set_series_darking, "lv_chart_set_series_darking");

   procedure init_points
     (Self : Instance;
      Ser  : Series;
      arg3 : LV.Area.Coord_T);  -- lv_chart.h:155
   pragma Import (C, init_points, "lv_chart_init_points");

   procedure set_points
     (Self : Instance;
      Ser  : Series;
      arg3 : access LV.Area.Coord_T);  -- lv_chart.h:163
   pragma Import (C, set_points, "lv_chart_set_points");

   procedure set_next
     (Self : Instance;
      Ser  : Series;
      arg3 : LV.Area.Coord_T);  -- lv_chart.h:171
   pragma Import (C, set_next, "lv_chart_set_next");

   procedure set_style (Self : Instance; style : LV.Style.Style);  -- lv_chart.h:178
   pragma Import (C, set_style, "lv_chart_set_style_inline");

   function get_type (Self : Instance) return lv_chart_type_t;  -- lv_chart.h:192
   pragma Import (C, get_type, "lv_chart_get_type");

   function get_point_cnt (Self : Instance) return uint16_t;  -- lv_chart.h:199
   pragma Import (C, get_point_cnt, "lv_chart_get_point_cnt");

   function get_series_opa (Self : Instance) return LV.Color.lv_opa_t;  -- lv_chart.h:206
   pragma Import (C, get_series_opa, "lv_chart_get_series_opa");

   function get_series_width (Self : Instance) return LV.Area.Coord_T;  -- lv_chart.h:213
   pragma Import (C, get_series_width, "lv_chart_get_series_width");

   function get_series_darking (Self : Instance) return LV.Color.lv_opa_t;  -- lv_chart.h:220
   pragma Import (C, get_series_darking, "lv_chart_get_series_darking");

   function get_style (Self : Instance) return LV.Style.Style;  -- lv_chart.h:227
   pragma Import (C, get_style, "lv_chart_get_style_inline");

   procedure refresh (Self : Instance);  -- lv_chart.h:240
   pragma Import (C, refresh, "lv_chart_refresh");

private

   type Series is new System.Address;

--     type lv_chart_ext_t;
--     type lv_chart_ext_t_series_struct is record
--        width : aliased LV.Area.Coord_T;  -- lv_chart.h:53
--        num : aliased uint8_t;  -- lv_chart.h:54
--        opa : aliased LV.Color.lv_opa_t;  -- lv_chart.h:55
--        dark : aliased lv_color_h.lv_opa_t;  -- lv_chart.h:56
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_chart_ext_t_series_struct);
--     type lv_chart_ext_t is record
--        series_ll : aliased lv_ll_h.lv_ll_t;  -- lv_chart.h:45
--        ymin : aliased lv_area_h.Coord_T;  -- lv_chart.h:46
--        ymax : aliased lv_area_h.Coord_T;  -- lv_chart.h:47
--        hdiv_cnt : aliased sys_ustdint_h.uint8_t;  -- lv_chart.h:48
--        vdiv_cnt : aliased sys_ustdint_h.uint8_t;  -- lv_chart.h:49
--        point_cnt : aliased sys_ustdint_h.uint16_t;  -- lv_chart.h:50
--        c_type : Extensions.Unsigned_3;  -- lv_chart.h:51
--        series : aliased lv_chart_ext_t_series_struct;  -- lv_chart.h:57
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_chart_ext_t);
--     pragma Pack (lv_chart_ext_t);  -- lv_chart.h:58
--     type lv_chart_series_t is record
--        points : access lv_area_h.Coord_T;  -- lv_chart.h:36
--        color : aliased lv_color_h.lv_color_t;  -- lv_chart.h:37
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_chart_series_t);  -- lv_chart.h:38

end LV.Objx.Chart;
