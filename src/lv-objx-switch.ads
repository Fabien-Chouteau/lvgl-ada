pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

with LV.Style;

package LV.Objx.Switch is

   subtype Instance is Obj_T;

   subtype style_t is uint8_t;  -- lv_sw.h:55

   function create (Parent : Obj_T; Copy : Obj_T) return Instance;  -- lv_sw.h:67
   pragma Import (C, create, "lv_sw_create");

   procedure on (Self : Instance);  -- lv_sw.h:77
   pragma Import (C, on, "lv_sw_on");

   procedure off (Self : Instance);  -- lv_sw.h:83
   pragma Import (C, off, "lv_sw_off");

   procedure set_action (Self : Instance; action : lv_action_t);  -- lv_sw.h:90
   pragma Import (C, set_action, "lv_sw_set_action_inline");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_sw.h:101
   pragma Import (C, set_style, "lv_sw_set_style");

   function get_state (Self : Instance) return u_Bool;  -- lv_sw.h:112
   pragma Import (C, get_state, "lv_sw_get_state_inline");

   function get_action (Self : Instance) return lv_action_t;  -- lv_sw.h:122
   pragma Import (C, get_action, "lv_sw_get_action_inline");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_sw.h:133
   pragma Import (C, get_style, "lv_sw_get_style");

--  private
--     type lv_sw_ext_t is record
--        slider : aliased lv_slider_h.lv_slider_ext_t;  -- lv_sw.h:42
--        style_knob_off : access lv_style_h.lv_style_t;  -- lv_sw.h:44
--        style_knob_on : access lv_style_h.lv_style_t;  -- lv_sw.h:45
--        changed : Extensions.Unsigned_1;  -- lv_sw.h:46
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_sw_ext_t);
--     pragma Pack (lv_sw_ext_t);  -- lv_sw.h:47

end LV.Objx.Switch;
