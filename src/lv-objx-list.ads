pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with LV.Style;
with System;
with Interfaces.C.Strings;

with LV.Objx.Label;
with LV.Objx.Page;
with LV.Objx.Btn;

package LV.Objx.List is

   subtype Instance is Obj_T;

   subtype lv_list_style_t is uint8_t;

   function create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, create, "lv_list_create");

   procedure clean (Self : Instance);
   pragma Import (C, clean, "lv_list_clean");

   function add
     (Self : Instance;
      arg2 : System.Address;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : lv_action_t) return Btn.Instance;
   pragma Import (C, add, "lv_list_add");

   procedure set_btn_selected (Self : Instance; arg2 : Obj_T);  -- lv_list.h:119
   pragma Import (C, set_btn_selected, "lv_list_set_btn_selected");

   procedure set_anim_time (Self : Instance; arg2 : uint16_t);  -- lv_list.h:127
   pragma Import (C, set_anim_time, "lv_list_set_anim_time");

   procedure set_sb_mode (Self : Instance; mode : LV.Objx.Page.lv_sb_mode_t);  -- lv_list.h:134
   pragma Import (C, set_sb_mode, "lv_list_set_sb_mode_inline");

   procedure set_style
     (Self : Instance;
      arg2 : lv_list_style_t;
      arg3 : LV.Style.Style);  -- lv_list.h:145
   pragma Import (C, set_style, "lv_list_set_style");

   function get_btn_text (Self : Instance) return Interfaces.C.Strings.chars_ptr;  -- lv_list.h:156
   pragma Import (C, get_btn_text, "lv_list_get_btn_text");

   function get_btn_label (Self : Instance) return Label.Instance;  -- lv_list.h:162
   pragma Import (C, get_btn_label, "lv_list_get_btn_label");

   function get_btn_img (Self : Instance) return Obj_T;  -- lv_list.h:169
   pragma Import (C, get_btn_img, "lv_list_get_btn_img");

   function get_prev_btn (Self : Instance; arg2 : Obj_T) return Obj_T;  -- lv_list.h:177
   pragma Import (C, get_prev_btn, "lv_list_get_prev_btn");

   function get_next_btn (Self : Instance; arg2 : Obj_T) return Obj_T;  -- lv_list.h:185
   pragma Import (C, get_next_btn, "lv_list_get_next_btn");

   function get_btn_selected (Self : Instance) return Obj_T;  -- lv_list.h:193
   pragma Import (C, get_btn_selected, "lv_list_get_btn_selected");

   function get_anim_time (Self : Instance) return uint16_t;  -- lv_list.h:202
   pragma Import (C, get_anim_time, "lv_list_get_anim_time");

   function get_sb_mode (Self : Instance) return LV.Objx.Page.lv_sb_mode_t;  -- lv_list.h:210
   pragma Import (C, get_sb_mode, "lv_list_get_sb_mode_inline");

   function get_style (Self : Instance; arg2 : lv_list_style_t) return LV.Style.Style;  -- lv_list.h:221
   pragma Import (C, get_style, "lv_list_get_style");

   procedure up (Self : Instance);  -- lv_list.h:231
   pragma Import (C, up, "lv_list_up");

   procedure down (Self : Instance);  -- lv_list.h:236
   pragma Import (C, down, "lv_list_down");

   procedure focus (Self : Instance; arg2 : u_Bool);  -- lv_list.h:243
   pragma Import (C, focus, "lv_list_focus");

--  private
--     type lv_list_ext_t_styles_btn_array is array (0 .. 4) of access LV.Style.lv_style_t;
--     type lv_list_ext_t is record
--        page : aliased lv_page_h.lv_page_ext_t;  -- lv_list.h:54
--        anim_time : aliased uint16_t;  -- lv_list.h:56
--        styles_btn : lv_list_ext_t_styles_btn_array;  -- lv_list.h:57
--        style_img : access LV.Style.lv_style_t;  -- lv_list.h:58
--        selected_btn : access lv_obj_h.lv_obj_t;  -- lv_list.h:60
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_list_ext_t);  -- lv_list.h:62

end LV.Objx.List;
