pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with LV.Style;

package LV.Objx.Bar is

   subtype Instance is Obj_T;

   subtype style_t is uint8_t;  -- lv_bar.h:52

   function create (Parent : Instance; Copy : Obj_T) return Instance;  -- lv_bar.h:64
   pragma Import (C, create, "lv_bar_create");

   procedure set_value (Self : Instance; arg2 : int16_t);  -- lv_bar.h:75
   pragma Import (C, set_value, "lv_bar_set_value");

   procedure set_value_anim
     (Self : Instance;
      arg2 : int16_t;
      arg3 : uint16_t);  -- lv_bar.h:83
   pragma Import (C, set_value_anim, "lv_bar_set_value_anim");

   procedure set_range
     (Self : Instance;
      arg2 : int16_t;
      arg3 : int16_t);  -- lv_bar.h:92
   pragma Import (C, set_range, "lv_bar_set_range");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : LV.Style.Style);  -- lv_bar.h:100
   pragma Import (C, set_style, "lv_bar_set_style");

   function get_value (Self : Instance) return int16_t;  -- lv_bar.h:111
   pragma Import (C, get_value, "lv_bar_get_value");

   function get_min_value (Self : Instance) return int16_t;  -- lv_bar.h:118
   pragma Import (C, get_min_value, "lv_bar_get_min_value");

   function get_max_value (Self : Instance) return int16_t;  -- lv_bar.h:125
   pragma Import (C, get_max_value, "lv_bar_get_max_value");

   function get_style (Self : Instance; arg2 : style_t) return LV.Style.Style;  -- lv_bar.h:134
   pragma Import (C, get_style, "lv_bar_get_style");

--  private
--     type lv_bar_ext_t is record
--        cur_value : aliased int16_t;  -- lv_bar.h:42
--        min_value : aliased int16_t;  -- lv_bar.h:43
--        max_value : aliased int16_t;  -- lv_bar.h:44
--        style_indic : access LV.Style.lv_style_t;  -- lv_bar.h:45
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_bar_ext_t);  -- lv_bar.h:46

end LV.Objx.Bar;
