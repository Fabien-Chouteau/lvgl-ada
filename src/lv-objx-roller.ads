with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with LV.Style;

package LV.Objx.Roller is

   subtype Instance is Obj_T;

   subtype style_t is uint8_t;  -- lv_roller.h:49

   function create (Parent : Obj_T; Copy : Instance) return Instance;  -- lv_roller.h:61
   pragma Import (C, create, "lv_roller_create");

   procedure set_options (Self : Instance; options : Interfaces.C.Strings.chars_ptr);  -- lv_roller.h:72
   pragma Import (C, set_options, "lv_roller_set_options_inline");

   procedure set_selected
     (Self : Instance;
      arg2 : uint16_t;
      arg3 : u_Bool);  -- lv_roller.h:83
   pragma Import (C, set_selected, "lv_roller_set_selected");

   procedure set_action (Self : Instance; Action : lv_action_t);  -- lv_roller.h:91
   pragma Import (C, set_action, "lv_roller_set_action_inline");

   procedure set_visible_row_count (Self : Instance; arg2 : uint8_t);  -- lv_roller.h:101
   pragma Import (C, set_visible_row_count, "lv_roller_set_visible_row_count");

   procedure set_hor_fit (Self : Instance; fit_en : u_Bool);  -- lv_roller.h:108
   pragma Import (C, set_hor_fit, "lv_roller_set_hor_fit_inline");

   procedure set_anim_time (Self : Instance; anim_time : uint16_t);  -- lv_roller.h:118
   pragma Import (C, set_anim_time, "lv_roller_set_anim_time_inline");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_roller.h:130
   pragma Import (C, set_style, "lv_roller_set_style");

   function get_options (Self : Instance) return Interfaces.C.Strings.chars_ptr;  -- lv_roller.h:141
   pragma Import (C, get_options, "lv_roller_get_options_inline");

   function get_selected (Self : Instance) return uint16_t;  -- lv_roller.h:151
   pragma Import (C, get_selected, "lv_roller_get_selected_inline");

   procedure get_selected_str (Self : Instance; buf : Interfaces.C.Strings.chars_ptr);  -- lv_roller.h:161
   pragma Import (C, get_selected_str, "lv_roller_get_selected_str_inline");

   function get_action (Self : Instance) return lv_action_t;  -- lv_roller.h:171
   pragma Import (C, get_action, "lv_roller_get_action_inline");

   function get_anim_time (Self : Instance) return uint16_t;  -- lv_roller.h:181
   pragma Import (C, get_anim_time, "lv_roller_get_anim_time_inline");

   function get_hor_fit (Self : Instance) return u_Bool;  -- lv_roller.h:191
   pragma Import (C, get_hor_fit, "lv_roller_get_hor_fit");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_roller.h:199
   pragma Import (C, get_style, "lv_roller_get_style");

--  private
--     type lv_roller_ext_t is record
--        ddlist : aliased lv_ddlist_h.lv_ddlist_ext_t;  -- lv_roller.h:41
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_roller_ext_t);  -- lv_roller.h:43

end LV.Objx.Roller;
