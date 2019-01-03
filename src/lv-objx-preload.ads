with Interfaces.C; use Interfaces.C;
with LV.Style;
with System;

package LV.Objx.Preload is

   subtype Instance is Obj_T;

   subtype lv_preloader_type_t is uint8_t;

   subtype style_t is uint8_t;

   function create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, create, "lv_preload_create");

   procedure set_arc_length (Self : Instance; arg2 : uint16_t);  -- lv_preload.h:85
   pragma Import (C, set_arc_length, "lv_preload_set_arc_length");

   procedure set_spin_time (Self : Instance; arg2 : uint16_t);  -- lv_preload.h:92
   pragma Import (C, set_spin_time, "lv_preload_set_spin_time");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_preload.h:104
   pragma Import (C, set_style, "lv_preload_set_style");

   function get_arc_length (Self : Instance) return uint16_t;  -- lv_preload.h:114
   pragma Import (C, get_arc_length, "lv_preload_get_arc_length");

   function get_spin_time (Self : Instance) return uint16_t;  -- lv_preload.h:120
   pragma Import (C, get_spin_time, "lv_preload_get_spin_time");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_preload.h:128
   pragma Import (C, get_style, "lv_preload_get_style");

   procedure spinner_animation (arg1 : System.Address; arg2 : int32_t);  -- lv_preload.h:140
   pragma Import (C, spinner_animation, "lv_preload_spinner_animation");

--  private
--     type lv_preload_ext_t is record
--        arc : aliased lv_arc_h.lv_arc_ext_t;  -- lv_preload.h:51
--        arc_length : aliased uint16_t;  -- lv_preload.h:53
--        time : aliased uint16_t;  -- lv_preload.h:54
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_preload_ext_t);  -- lv_preload.h:55

end LV.Objx.Preload;
