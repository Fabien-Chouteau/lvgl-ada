pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with LV.Style;
with System;
with Interfaces.C.Extensions;

package LV.Objx.Btnm is

   subtype Instance is Obj_T;

   LV_BTNM_CTRL_CODE : constant := 16#80#;  --  lv_btnm.h:34
   LV_BTNM_CTRL_MASK : constant := 16#C0#;  --  lv_btnm.h:35
   LV_BTNM_WIDTH_MASK : constant := 16#07#;  --  lv_btnm.h:36
   LV_BTNM_HIDE_MASK : constant := 16#08#;  --  lv_btnm.h:37
   LV_BTNM_REPEAT_DISABLE_MASK : constant := 16#10#;  --  lv_btnm.h:38
   LV_BTNM_INACTIVE_MASK : constant := 16#20#;  --  lv_btnm.h:39

   LV_BTNM_PR_NONE : constant := 16#FFFF#;  --  lv_btnm.h:42

   type action_t is access function (arg1 : access Obj_T; arg2 : Interfaces.C.Strings.chars_ptr) return lv_res_t;
   pragma Convention (C, action_t);  -- lv_btnm.h:50

   subtype style_t is uint8_t;  -- lv_btnm.h:75

   function create (Parent : Obj_T; Copy : Obj_T) return Instance;  -- lv_btnm.h:87
   pragma Import (C, create, "lv_btnm_create");

   procedure set_map (Self : Instance; arg2 : System.Address);  -- lv_btnm.h:107
   pragma Import (C, set_map, "lv_btnm_set_map");

   procedure set_action (Self : Instance; arg2 : action_t);  -- lv_btnm.h:114
   pragma Import (C, set_action, "lv_btnm_set_action");

   procedure set_toggle
     (Self : Instance;
      arg2 : u_Bool;
      arg3 : uint16_t);  -- lv_btnm.h:122
   pragma Import (C, set_toggle, "lv_btnm_set_toggle");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : LV.Style.Style);  -- lv_btnm.h:130
   pragma Import (C, set_style, "lv_btnm_set_style");

   function get_map (Self : Instance) return System.Address;  -- lv_btnm.h:141
   pragma Import (C, get_map, "lv_btnm_get_map");

   function get_action (Self : Instance) return action_t;  -- lv_btnm.h:148
   pragma Import (C, get_action, "lv_btnm_get_action");

   function get_toggled (Self : Instance) return uint16_t;  -- lv_btnm.h:156
   pragma Import (C, get_toggled, "lv_btnm_get_toggled");

   function get_style (Self : Instance; arg2 : style_t) return LV.Style.Style;  -- lv_btnm.h:164
   pragma Import (C, get_style, "lv_btnm_get_style");

--  private
--
--     type lv_btnm_ext_t_styles_btn_array is array (0 .. 4) of access LV.Style.lv_style_t;
--     type lv_btnm_ext_t is record
--        map_p : System.Address;  -- lv_btnm.h:57
--        button_areas : access LV.Area.lv_area_t;  -- lv_btnm.h:58
--        action : action_t;  -- lv_btnm.h:59
--        styles_btn : ext_t_styles_btn_array;  -- lv_btnm.h:60
--        btn_cnt : aliased uint16_t;  -- lv_btnm.h:61
--        btn_id_pr : aliased sys_ustdint_h.uint16_t;  -- lv_btnm.h:62
--        btn_id_tgl : aliased sys_ustdint_h.uint16_t;  -- lv_btnm.h:63
--        toggle : Extensions.Unsigned_1;  -- lv_btnm.h:64
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_btnm_ext_t);
--     pragma Pack (lv_btnm_ext_t);  -- lv_btnm.h:65

end LV.Objx.Btnm;
