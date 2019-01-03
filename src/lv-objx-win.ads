with Interfaces.C; use Interfaces.C;
with LV.Style;
with LV.Area;
with LV.Objx.Page;
with LV.Objx.Btn;
with LV.Objx.Cont;
with System;
with Interfaces.C.Strings;

package LV.Objx.Win is

   subtype Instance is Obj_T;

   subtype style_t is uint8_t;  -- lv_win.h:80

   function create (Parent : Obj_T; Copy : Instance) return Instance;  -- lv_win.h:92
   pragma Import (C, create, "lv_win_create");

   procedure clean (Self : Instance);  -- lv_win.h:98
   pragma Import (C, clean, "lv_win_clean");

   function add_btn
     (Self : Instance;
      arg2 : System.Address;
      arg3 : lv_action_t) return Btn.Instance;  -- lv_win.h:111
   pragma Import (C, add_btn, "lv_win_add_btn");

   function close_action (Self : Instance) return lv_res_t;  -- lv_win.h:122
   pragma Import (C, close_action, "lv_win_close_action");

   procedure set_title (Self : Instance; arg2 : Interfaces.C.Strings.chars_ptr);  -- lv_win.h:129
   pragma Import (C, set_title, "lv_win_set_title");

   procedure set_btn_size (Self : Instance; arg2 : LV.Area.Coord_T);  -- lv_win.h:136
   pragma Import (C, set_btn_size, "lv_win_set_btn_size");

   procedure set_layout (Self : Instance; arg2 : LV.Objx.Cont.lv_layout_t);  -- lv_win.h:143
   pragma Import (C, set_layout, "lv_win_set_layout");

   procedure set_sb_mode (Self : Instance; arg2 : LV.Objx.Page.lv_sb_mode_t);  -- lv_win.h:150
   pragma Import (C, set_sb_mode, "lv_win_set_sb_mode");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_win.h:158
   pragma Import (C, set_style, "lv_win_set_style");

   function get_title (Self : Instance) return Interfaces.C.Strings.chars_ptr;  -- lv_win.h:170
   pragma Import (C, get_title, "lv_win_get_title");

   function get_content (Self : Instance) return Page.Instance;  -- lv_win.h:177
   pragma Import (C, get_content, "lv_win_get_content");

   function get_btn_size (Self : Instance) return LV.Area.Coord_T;  -- lv_win.h:184
   pragma Import (C, get_btn_size, "lv_win_get_btn_size");

   function get_from_btn (Ctrl_Btn : Btn.Instance) return Instance;  -- lv_win.h:192
   pragma Import (C, get_from_btn, "lv_win_get_from_btn");

   function get_layout (Self : Instance) return LV.Objx.Cont.lv_layout_t;  -- lv_win.h:199
   pragma Import (C, get_layout, "lv_win_get_layout");

   function get_sb_mode (Self : Instance) return LV.Objx.Page.lv_sb_mode_t;  -- lv_win.h:206
   pragma Import (C, get_sb_mode, "lv_win_get_sb_mode");

   function get_width (Self : Instance) return LV.Area.Coord_T;  -- lv_win.h:213
   pragma Import (C, get_width, "lv_win_get_width");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_win.h:221
   pragma Import (C, get_style, "lv_win_get_style");

   procedure focus
     (Self      : Instance;
      Obj       : Obj_T;
      Anim_Time : uint16_t);  -- lv_win.h:233
   pragma Import (C, focus, "lv_win_focus");

   procedure scroll_hor (Self : Instance; dist : LV.Area.Coord_T);  -- lv_win.h:240
   pragma Import (C, scroll_hor, "lv_win_scroll_hor_inline");

   procedure scroll_ver (Self : Instance; dist : LV.Area.Coord_T);  -- lv_win.h:250
   pragma Import (C, scroll_ver, "lv_win_scroll_ver_inline");

--  private
--     type lv_win_ext_t is record
--        page : access lv_obj_h.lv_obj_t;  -- lv_win.h:62
--        header : access lv_obj_h.lv_obj_t;  -- lv_win.h:63
--        title : access lv_obj_h.lv_obj_t;  -- lv_win.h:64
--        style_header : access lv_style_h.lv_style_t;  -- lv_win.h:65
--        style_btn_rel : access lv_style_h.lv_style_t;  -- lv_win.h:66
--        style_btn_pr : access lv_style_h.lv_style_t;  -- lv_win.h:67
--        btn_size : aliased LV.Area.Coord_T;  -- lv_win.h:68
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_win_ext_t);  -- lv_win.h:69

end LV.Objx.Win;
