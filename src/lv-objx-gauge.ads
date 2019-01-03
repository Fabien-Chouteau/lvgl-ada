with Interfaces.C; use Interfaces.C;
with LV.Color;
with LV.Style;

package LV.Objx.Gauge is

   subtype Instance is Obj_T;

   function create (Parent : Obj_T; Copy : Instance) return Instance;  -- lv_gauge.h:63
   pragma Import (C, create, "lv_gauge_create");

   procedure set_needle_count
     (Self : Instance;
      arg2 : uint8_t;
      arg3 : access constant LV.Color.lv_color_t);  -- lv_gauge.h:75
   pragma Import (C, set_needle_count, "lv_gauge_set_needle_count");

   procedure set_value
     (Self      : Instance;
      Needle_Id : uint8_t;
      Value     : int16_t);
   pragma Import (C, set_value, "lv_gauge_set_value");

   procedure set_range
     (Self : Instance;
      min : int16_t;
      max : int16_t);  -- lv_gauge.h:91
   pragma Import (C, set_range, "lv_gauge_set_range_inline");

   procedure set_critical_value (Self : Instance; value : int16_t);  -- lv_gauge.h:101
   pragma Import (C, set_critical_value, "lv_gauge_set_critical_value_inline");

   procedure set_scale
     (Self : Instance;
      arg2 : uint16_t;
      arg3 : uint8_t;
      arg4 : uint8_t);  -- lv_gauge.h:114
   pragma Import (C, set_scale, "lv_gauge_set_scale");

   procedure set_style (Self : Instance; bg : access LV.Style.Style);  -- lv_gauge.h:121
   pragma Import (C, set_style, "lv_gauge_set_style_inline");

   function get_value (Self : Instance; arg2 : uint8_t) return int16_t;  -- lv_gauge.h:136
   pragma Import (C, get_value, "lv_gauge_get_value");

   function get_needle_count (Self : Instance) return uint8_t;  -- lv_gauge.h:143
   pragma Import (C, get_needle_count, "lv_gauge_get_needle_count");

   function get_min_value (Self : Instance) return int16_t;  -- lv_gauge.h:150
   pragma Import (C, get_min_value, "lv_gauge_get_min_value_inline");

   function get_max_value (Self : Instance) return int16_t;  -- lv_gauge.h:160
   pragma Import (C, get_max_value, "lv_gauge_get_max_value_inline");

   function get_critical_value (Self : Instance) return int16_t;  -- lv_gauge.h:170
   pragma Import (C, get_critical_value, "lv_gauge_get_critical_value_inline");

   function get_label_count (Self : Instance) return uint8_t;  -- lv_gauge.h:180
   pragma Import (C, get_label_count, "lv_gauge_get_label_count");

   function get_line_count (Self : Instance) return uint8_t;  -- lv_gauge.h:187
   pragma Import (C, get_line_count, "lv_gauge_get_line_count_inline");

   function get_scale_angle (Self : Instance) return uint16_t;  -- lv_gauge.h:197
   pragma Import (C, get_scale_angle, "lv_gauge_get_scale_angle_inline");

   function get_style (Self : Instance) return access LV.Style.Style;  -- lv_gauge.h:207
   pragma Import (C, get_style, "lv_gauge_get_style_inline");

--  private
--     type lv_gauge_ext_t is record
--        lmeter : aliased lv_lmeter_h.lv_lmeter_ext_t;  -- lv_gauge.h:45
--        values : access int16_t;  -- lv_gauge.h:47
--        needle_colors : access constant lv_color_h.lv_color_t;  -- lv_gauge.h:48
--        needle_count : aliased sys_ustdint_h.uint8_t;  -- lv_gauge.h:49
--        label_count : aliased sys_ustdint_h.uint8_t;  -- lv_gauge.h:50
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_gauge_ext_t);  -- lv_gauge.h:51

end LV.Objx.Gauge;
