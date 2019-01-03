pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with LV.Objx.Btnm;
with Interfaces.C.Extensions;
with System;
with LV.Style;

with LV.Objx.Textarea;

package LV.Objx.Keyboard is

   subtype Instance is Obj_T;

   type lv_kb_mode_t is
     (MODE_TEXT, MODE_NUM)
     with Size => 8;

   for lv_kb_mode_t use
     (MODE_TEXT => 0,
      MODE_NUM  => 1);

   type lv_kb_style_t is
     (STYLE_BG,
      STYLE_BTN_REL,
      STYLE_BTN_PR,
      STYLE_BTN_TGL_REL,
      STYLE_BTN_TGL_PR,
      STYLE_BTN_INA)
     with Size => 8;

   for lv_kb_style_t use
     (STYLE_BG          => 0,
      STYLE_BTN_REL     => 1,
      STYLE_BTN_PR      => 2,
      STYLE_BTN_TGL_REL => 3,
      STYLE_BTN_TGL_PR  => 4,
      STYLE_BTN_INA     => 5);

   function create (Par : Obj_T; Copy : Obj_T) return Instance;  -- lv_kb.h:82
   pragma Import (C, create, "lv_kb_create");

   procedure Set_Textarea (Self : Instance; TA : Textarea.Textarea);  -- lv_kb.h:93
   pragma Import (C, Set_Textarea, "lv_kb_set_ta");

   procedure set_mode (Self : Instance; arg2 : lv_kb_mode_t);  -- lv_kb.h:100
   pragma Import (C, set_mode, "lv_kb_set_mode");

   procedure set_cursor_manage (Self : Instance; arg2 : u_Bool);  -- lv_kb.h:107
   pragma Import (C, set_cursor_manage, "lv_kb_set_cursor_manage");

   procedure set_ok_action (Self : Instance; arg2 : lv_action_t);  -- lv_kb.h:114
   pragma Import (C, set_ok_action, "lv_kb_set_ok_action");

   procedure set_hide_action (Self : Instance; arg2 : lv_action_t);  -- lv_kb.h:121
   pragma Import (C, set_hide_action, "lv_kb_set_hide_action");

   procedure set_map (Self : Instance; map : System.Address);  -- lv_kb.h:129
   pragma Import (C, set_map, "lv_kb_set_map_inline");

   procedure set_style
     (Self : Instance;
      arg2 : lv_kb_style_t;
      arg3 : LV.Style.Style);  -- lv_kb.h:140
   pragma Import (C, set_style, "lv_kb_set_style");

   function get_textarea (Self : Instance) return Textarea.Textarea;  -- lv_kb.h:151
   pragma Import (C, get_textarea, "lv_kb_get_ta");

   function get_mode (Self : Instance) return lv_kb_mode_t;  -- lv_kb.h:158
   pragma Import (C, get_mode, "lv_kb_get_mode");

   function get_cursor_manage (Self : Instance) return u_Bool;  -- lv_kb.h:165
   pragma Import (C, get_cursor_manage, "lv_kb_get_cursor_manage");

   function get_ok_action (Self : Instance) return lv_action_t;  -- lv_kb.h:172
   pragma Import (C, get_ok_action, "lv_kb_get_ok_action");

   function get_hide_action (Self : Instance) return lv_action_t;  -- lv_kb.h:179
   pragma Import (C, get_hide_action, "lv_kb_get_hide_action");

   function get_style (Self : Instance; arg2 : lv_kb_style_t) return LV.Style.Style;  -- lv_kb.h:187
   pragma Import (C, get_style, "lv_kb_get_style");

--  private
--     type lv_kb_ext_t is record
--        btnm : aliased LV.Btnm.lv_btnm_ext_t;  -- lv_kb.h:52
--        ta : access lv_obj_h.lv_obj_t;  -- lv_kb.h:54
--        mode : aliased lv_kb_mode_t;  -- lv_kb.h:55
--        cursor_mng : Extensions.Unsigned_1;  -- lv_kb.h:56
--        ok_action : lv_obj_h.lv_action_t;  -- lv_kb.h:57
--        hide_action : lv_obj_h.lv_action_t;  -- lv_kb.h:58
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_kb_ext_t);
--     pragma Pack (lv_kb_ext_t);  -- lv_kb.h:59

end LV.Objx.Keyboard;
