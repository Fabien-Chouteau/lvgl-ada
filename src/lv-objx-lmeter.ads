with Interfaces.C; use Interfaces.C;
with LV.Style;

package LV.Objx.Lmeter is

   subtype Instance is Obj_T;

   function create (Parent : Obj_T; Copy : Instance) return Instance;  -- lv_lmeter.h:55
   pragma Import (C, create, "lv_lmeter_create");

   procedure set_value (Self : Instance; arg2 : int16_t);  -- lv_lmeter.h:66
   pragma Import (C, set_value, "lv_lmeter_set_value");

   procedure set_range
     (Self : Instance;
      arg2 : int16_t;
      arg3 : int16_t);  -- lv_lmeter.h:74
   pragma Import (C, set_range, "lv_lmeter_set_range");

   procedure set_scale
     (Self : Instance;
      arg2 : uint16_t;
      arg3 : uint8_t);  -- lv_lmeter.h:82
   pragma Import (C, set_scale, "lv_lmeter_set_scale");

   procedure set_style (Self : Instance; bg : access LV.Style.Style);  -- lv_lmeter.h:89
   pragma Import (C, set_style, "lv_lmeter_set_style_inline");

   function get_value (Self : Instance) return int16_t;  -- lv_lmeter.h:103
   pragma Import (C, get_value, "lv_lmeter_get_value");

   function get_min_value (Self : Instance) return int16_t;  -- lv_lmeter.h:110
   pragma Import (C, get_min_value, "lv_lmeter_get_min_value");

   function get_max_value (Self : Instance) return int16_t;  -- lv_lmeter.h:117
   pragma Import (C, get_max_value, "lv_lmeter_get_max_value");

   function get_line_count (Self : Instance) return uint8_t;  -- lv_lmeter.h:124
   pragma Import (C, get_line_count, "lv_lmeter_get_line_count");

   function get_scale_angle (Self : Instance) return uint16_t;  -- lv_lmeter.h:131
   pragma Import (C, get_scale_angle, "lv_lmeter_get_scale_angle");

   function get_style (Self : Instance) return access LV.Style.Style;  -- lv_lmeter.h:138
   pragma Import (C, get_style, "lv_lmeter_get_style_inline");

--  private
--     type lv_lmeter_ext_t is record
--        scale_angle : aliased uint16_t;  -- lv_lmeter.h:38
--        line_cnt : aliased sys_ustdint_h.uint8_t;  -- lv_lmeter.h:39
--        cur_value : aliased sys_ustdint_h.int16_t;  -- lv_lmeter.h:40
--        min_value : aliased sys_ustdint_h.int16_t;  -- lv_lmeter.h:41
--        max_value : aliased sys_ustdint_h.int16_t;  -- lv_lmeter.h:42
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_lmeter_ext_t);  -- lv_lmeter.h:43

end LV.Objx.Lmeter;
