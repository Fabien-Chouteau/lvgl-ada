pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with LV.Area;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with LV.Style;

package LV.Objx.Tabview is

   subtype Instance is Obj_T;

   type lv_tabview_action_t is access function (arg1 : access Obj_T; arg2 : uint16_t) return lv_res_t;
   pragma Convention (C, lv_tabview_action_t);  -- lv_tabview.h:47

   subtype lv_tabview_btns_pos_t is uint8_t;  -- lv_tabview.h:54

   type lv_tabview_style_t is
     (STYLE_BG,
      STYLE_INDIC,
      STYLE_BTN_BG,
      STYLE_BTN_REL,
      STYLE_BTN_PR,
      STYLE_BTN_TGL_REL,
      STYLE_BTN_TGL_PR)
   with Size => 8;
   --  subtype lv_tabview_style_t is uint8_t;  -- lv_tabview.h:85

   for lv_tabview_style_t use
     (STYLE_BG   => 0,
      STYLE_INDIC => 1,
      STYLE_BTN_BG => 2,
      STYLE_BTN_REL => 3,
      STYLE_BTN_PR => 4,
      STYLE_BTN_TGL_REL => 5,
      STYLE_BTN_TGL_PR => 6);

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;  -- lv_tabview.h:98
   pragma Import (C, Create, "lv_tabview_create");

   procedure Clean (Self : Instance);  -- lv_tabview.h:104
   pragma Import (C, Clean, "lv_tabview_clean");

   function Add_Tab (Self : Instance; Name : Interfaces.C.Strings.chars_ptr) return Obj_T;  -- lv_tabview.h:116
   pragma Import (C, Add_Tab, "lv_tabview_add_tab");

   procedure Add_Tab (Self : Instance; Name : Interfaces.C.Strings.chars_ptr)
     with Import        => True,
          Convention    => C,
          External_Name => "lv_tabview_add_tab";

   procedure Set_Tab_Act
     (Self    : Instance;
      Id      : uint16_t;
      Anim_en : u_Bool);  -- lv_tabview.h:128
   pragma Import (C, Set_Tab_Act, "lv_tabview_set_tab_act");

   procedure Set_Tab_Load_Action (Self : Instance; Action : lv_tabview_action_t);  -- lv_tabview.h:136
   pragma Import (C, Set_Tab_Load_Action, "lv_tabview_set_tab_load_action");

   procedure Set_Sliding (Self : Instance; En : u_Bool);  -- lv_tabview.h:143
   pragma Import (C, Set_Sliding, "lv_tabview_set_sliding");

   procedure Set_Anim_Time (Self : Instance; Anim_Time : uint16_t);  -- lv_tabview.h:150
   pragma Import (C, Set_Anim_Time, "lv_tabview_set_anim_time");

   procedure Set_Style
     (Self  : Instance;
      Typ   : lv_tabview_style_t;
      Style : LV.Style.Style);  -- lv_tabview.h:158
   pragma Import (C, set_style, "lv_tabview_set_style");

   procedure Set_Btns_Pos (Self : Instance; Btns_Pos : lv_tabview_btns_pos_t);  -- lv_tabview.h:165
   pragma Import (C, set_btns_pos, "lv_tabview_set_btns_pos");

   function get_tab_act (Self : Instance) return uint16_t;  -- lv_tabview.h:176
   pragma Import (C, get_tab_act, "lv_tabview_get_tab_act");

   function get_tab_count (Self : Instance) return uint16_t;  -- lv_tabview.h:183
   pragma Import (C, get_tab_count, "lv_tabview_get_tab_count");

   function get_tab (Self : Instance; Id : uint16_t) return Obj_T;  -- lv_tabview.h:190
   pragma Import (C, get_tab, "lv_tabview_get_tab");

   function get_tab_load_action (Self : Instance) return lv_tabview_action_t;  -- lv_tabview.h:197
   pragma Import (C, get_tab_load_action, "lv_tabview_get_tab_load_action");

   function get_sliding (Self : Instance) return u_Bool;  -- lv_tabview.h:204
   pragma Import (C, get_sliding, "lv_tabview_get_sliding");

   function get_anim_time (Self : Instance) return uint16_t;  -- lv_tabview.h:211
   pragma Import (C, get_anim_time, "lv_tabview_get_anim_time");

   function get_style (Self : Instance; Style_Type : lv_tabview_style_t) return LV.Style.Style;  -- lv_tabview.h:219
   pragma Import (C, get_style, "lv_tabview_get_style");

   function get_btns_pos (Self : Instance) return lv_tabview_btns_pos_t;  -- lv_tabview.h:225
   pragma Import (C, get_btns_pos, "lv_tabview_get_btns_pos");

--  private
--
--     type lv_tabview_ext_t is record
--        btns : access lv_obj_h.lv_obj_t;  -- lv_tabview.h:61
--        indic : access lv_obj_h.lv_obj_t;  -- lv_tabview.h:62
--        content : access lv_obj_h.lv_obj_t;  -- lv_tabview.h:63
--        tab_name_ptr : System.Address;  -- lv_tabview.h:64
--        point_last : aliased LV.Area.Point_T;  -- lv_tabview.h:65
--        tab_cur : aliased uint16_t;  -- lv_tabview.h:66
--        tab_cnt : aliased sys_ustdint_h.uint16_t;  -- lv_tabview.h:67
--        anim_time : aliased sys_ustdint_h.uint16_t;  -- lv_tabview.h:68
--        slide_enable : Extensions.Unsigned_1;  -- lv_tabview.h:69
--        draging : Extensions.Unsigned_1;  -- lv_tabview.h:70
--        drag_hor : Extensions.Unsigned_1;  -- lv_tabview.h:71
--        btns_pos : Extensions.Unsigned_1;  -- lv_tabview.h:72
--        tab_load_action : lv_tabview_action_t;  -- lv_tabview.h:73
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_tabview_ext_t);
--     pragma Pack (lv_tabview_ext_t);  -- lv_tabview.h:74

end LV.Objx.Tabview;
