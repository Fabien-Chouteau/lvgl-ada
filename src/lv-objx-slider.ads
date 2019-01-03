pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with LV.Style;
with Interfaces.C.Extensions;

package LV.Objx.Slider is

   subtype Instance is Obj_T;

   subtype lv_slider_style_t is uint8_t;  -- lv_slider.h:57

   function create (Par : Obj_T; Copy : Obj_T) return Instance;  -- lv_slider.h:69
   pragma Import (C, create, "lv_slider_create");

   procedure set_value (Self : Instance; value : int16_t);  -- lv_slider.h:80
   pragma Import (C, set_value, "lv_slider_set_value_inline");

   procedure set_value_anim
     (Self      : Instance;
      value     : int16_t;
      anim_time : uint16_t);  -- lv_slider.h:91
   pragma Import (C, set_value_anim, "lv_slider_set_value_anim_inline");

   procedure set_range
     (Self : Instance;
      min : int16_t;
      max : int16_t);  -- lv_slider.h:102
   pragma Import (C, set_range, "lv_slider_set_range_inline");

   procedure set_action (Self : Instance; arg2 : lv_action_t);  -- lv_slider.h:112
   pragma Import (C, set_action, "lv_slider_set_action");

   procedure set_knob_in (Self : Instance; arg2 : u_Bool);  -- lv_slider.h:120
   pragma Import (C, set_knob_in, "lv_slider_set_knob_in");

   procedure set_style
     (Self : Instance;
      arg2 : lv_slider_style_t;
      arg3 : access LV.Style.Style);  -- lv_slider.h:128
   pragma Import (C, set_style, "lv_slider_set_style");

   function get_value (Self : Instance) return int16_t;  -- lv_slider.h:139
   pragma Import (C, get_value, "lv_slider_get_value");

   function get_min_value (Self : Instance) return int16_t;  -- lv_slider.h:146
   pragma Import (C, get_min_value, "lv_slider_get_min_value_inline");

   function get_max_value (Self : Instance) return int16_t;  -- lv_slider.h:156
   pragma Import (C, get_max_value, "lv_slider_get_max_value_inline");

   function get_action (Self : Instance) return lv_action_t;  -- lv_slider.h:166
   pragma Import (C, get_action, "lv_slider_get_action");

   function is_dragged (Self : Instance) return u_Bool;  -- lv_slider.h:173
   pragma Import (C, is_dragged, "lv_slider_is_dragged");

   function get_knob_in (Self : Instance) return u_Bool;  -- lv_slider.h:181
   pragma Import (C, get_knob_in, "lv_slider_get_knob_in");

   function get_style (Self : Instance; arg2 : lv_slider_style_t) return access LV.Style.Style;  -- lv_slider.h:190
   pragma Import (C, get_style, "lv_slider_get_style");

--  private
--     type lv_slider_ext_t is record
--        bar : aliased lv_bar_h.lv_bar_ext_t;  -- lv_slider.h:42
--        action : lv_obj_h.lv_action_t;  -- lv_slider.h:44
--        style_knob : access LV.Style.lv_style_t;  -- lv_slider.h:45
--        drag_value : aliased int16_t;  -- lv_slider.h:46
--        knob_in : Extensions.Unsigned_1;  -- lv_slider.h:47
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_slider_ext_t);
--     pragma Pack (lv_slider_ext_t);  -- lv_slider.h:48

end LV.Objx.Slider;
