pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with LV.Objx.Btnm;
with Interfaces.C.Strings;
with LV.Style;

package LV.Objx.Mbox is

   subtype Instance is Obj_T;

   subtype style_t is uint8_t;  -- lv_mbox.h:70

   function create (Par : Obj_T; Copy : Obj_T) return Instance;  -- lv_mbox.h:82
   pragma Import (C, create, "lv_mbox_create");

   procedure add_btns
     (Self    : Instance;
      Btn_Map : System.Address;
      Action  : LV.Objx.Btnm.action_t);  -- lv_mbox.h:95
   pragma Import (C, add_btns, "lv_mbox_add_btns");

   procedure set_text (Self : Instance; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_mbox.h:106
   pragma Import (C, set_text, "lv_mbox_set_text");

   procedure set_action (Self : Instance; arg2 : LV.Objx.Btnm.action_t);  -- lv_mbox.h:113
   pragma Import (C, set_action, "lv_mbox_set_action");

   procedure set_anim_time (Self : Instance; arg2 : uint16_t);  -- lv_mbox.h:120
   pragma Import (C, set_anim_time, "lv_mbox_set_anim_time");

   procedure start_auto_close (Self : Instance; arg2 : uint16_t);  -- lv_mbox.h:127
   pragma Import (C, start_auto_close, "lv_mbox_start_auto_close");

   procedure stop_auto_close (Self : Instance);  -- lv_mbox.h:133
   pragma Import (C, stop_auto_close, "lv_mbox_stop_auto_close");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : LV.Style.Style);  -- lv_mbox.h:141
   pragma Import (C, set_style, "lv_mbox_set_style");

   function get_text (Self : Instance) return Interfaces.C.Strings.chars_ptr;  -- lv_mbox.h:152
   pragma Import (C, get_text, "lv_mbox_get_text");

   function get_from_btn (Button : Obj_T) return Instance;  -- lv_mbox.h:160
   pragma Import (C, get_from_btn, "lv_mbox_get_from_btn");

   function get_anim_time (Self : Instance) return uint16_t;  -- lv_mbox.h:167
   pragma Import (C, get_anim_time, "lv_mbox_get_anim_time");

   function get_style (Self : Instance; arg2 : style_t) return LV.Style.Style;  -- lv_mbox.h:176
   pragma Import (C, get_style, "lv_mbox_get_style");

--  private
--     type lv_mbox_ext_t is record
--        bg : aliased LV.Cont.lv_cont_ext_t;  -- lv_mbox.h:54
--        text : access lv_obj_h.lv_obj_t;  -- lv_mbox.h:56
--        btnm : access lv_obj_h.lv_obj_t;  -- lv_mbox.h:57
--        anim_time : aliased sys_ustdint_h.uint16_t;  -- lv_mbox.h:58
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_mbox_ext_t);  -- lv_mbox.h:59
end LV.Objx.Mbox;
