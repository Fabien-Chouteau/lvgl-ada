with System;
with LV.Area;
with Interfaces.C.Strings;
with LV.Style;
with LV.Color; use LV.Color;
with Interfaces.C.Extensions;

package LV.Objx is

   type Obj_T is new System.Address;

   No_Obj : constant Obj_T := Obj_T (System.Null_Address);

   ANIM_IN : constant := 16#00#;  --  lv_obj.h:57
   ANIM_OUT : constant := 16#80#;  --  lv_obj.h:58
   ANIM_DIR_MASK : constant := 16#80#;  --  lv_obj.h:59

   LV_MAX_ANCESTOR_NUM : constant := 8;  --  lv_obj.h:61

   RES_INV : constant := 0;
   RES_OK  : constant := 1;

   type lv_align_t is
     (
      ALIGN_CENTER,
      ALIGN_IN_TOP_LEFT,
      ALIGN_IN_TOP_MID,
      ALIGN_IN_TOP_RIGHT,
      ALIGN_IN_BOTTOM_LEFT,
      ALIGN_IN_BOTTOM_MID,
      ALIGN_IN_BOTTOM_RIGHT,
      ALIGN_IN_LEFT_MID,
      ALIGN_IN_RIGHT_MID,
      ALIGN_OUT_TOP_LEFT,
      ALIGN_OUT_TOP_MID,
      ALIGN_OUT_TOP_RIGHT,
      ALIGN_OUT_BOTTOM_LEFT,
      ALIGN_OUT_BOTTOM_MID,
      ALIGN_OUT_BOTTOM_RIGHT,
      ALIGN_OUT_LEFT_TOP,
      ALIGN_OUT_LEFT_MID,
      ALIGN_OUT_LEFT_BOTTOM,
      ALIGN_OUT_RIGHT_TOP,
      ALIGN_OUT_RIGHT_MID,
      ALIGN_OUT_RIGHT_BOTTOM
     );

   for lv_align_t use
     (
      ALIGN_CENTER           => 0,
      ALIGN_IN_TOP_LEFT      => 1,
      ALIGN_IN_TOP_MID       => 2,
      ALIGN_IN_TOP_RIGHT     => 3,
      ALIGN_IN_BOTTOM_LEFT   => 4,
      ALIGN_IN_BOTTOM_MID    => 5,
      ALIGN_IN_BOTTOM_RIGHT  => 6,
      ALIGN_IN_LEFT_MID      => 7,
      ALIGN_IN_RIGHT_MID     => 8,
      ALIGN_OUT_TOP_LEFT     => 9,
      ALIGN_OUT_TOP_MID      => 10,
      ALIGN_OUT_TOP_RIGHT    => 11,
      ALIGN_OUT_BOTTOM_LEFT  => 12,
      ALIGN_OUT_BOTTOM_MID   => 13,
      ALIGN_OUT_BOTTOM_RIGHT => 14,
      ALIGN_OUT_LEFT_TOP     => 15,
      ALIGN_OUT_LEFT_MID     => 16,
      ALIGN_OUT_LEFT_BOTTOM  => 17,
      ALIGN_OUT_RIGHT_TOP    => 18,
      ALIGN_OUT_RIGHT_MID    => 19,
      ALIGN_OUT_RIGHT_BOTTOM => 20);

   subtype lv_design_mode_t is uint8_t;  -- lv_obj.h:74

   type lv_design_func_t is access function
        (arg1 : System.Address;
         arg2 : access constant LV.Area.Area_T;
         arg3 : lv_design_mode_t) return u_Bool;
   pragma Convention (C, lv_design_func_t);  -- lv_obj.h:76

   subtype lv_res_t is uint8_t;  -- lv_obj.h:83

   subtype lv_signal_t is uint8_t;  -- lv_obj.h:111

   type lv_signal_func_t is access function
        (arg1 : System.Address;
         arg2 : lv_signal_t;
         arg3 : System.Address) return lv_res_t;
   pragma Convention (C, lv_signal_func_t);  -- lv_obj.h:113

   type lv_action_t is access function (arg1 : Obj_T) return lv_res_t;
   pragma Convention (C, lv_action_t);  -- lv_obj.h:153

   subtype lv_protect_t is uint8_t;  -- lv_obj.h:166

   type lv_obj_type_t_c_type_array is array (0 .. 7) of Interfaces.C.Strings.chars_ptr;
   type lv_obj_type_t is record
      c_type : lv_obj_type_t_c_type_array;  -- lv_obj.h:171
   end record;
   pragma Convention (C_Pass_By_Copy, lv_obj_type_t);  -- lv_obj.h:172

   subtype lv_anim_builtin_t is uint8_t;  -- lv_obj.h:166
   ANIM_NONE         : constant lv_anim_builtin_t := 0;
   ANIM_FLOAT_TOP    : constant lv_anim_builtin_t := 1;
   ANIM_FLOAT_LEFT   : constant lv_anim_builtin_t := 2;
   ANIM_FLOAT_BOTTOM : constant lv_anim_builtin_t := 3;
   ANIM_FLOAT_RIGHT  : constant lv_anim_builtin_t := 4;
   ANIM_GROW_H       : constant lv_anim_builtin_t := 5;
   ANIM_GROW_V       : constant lv_anim_builtin_t := 6;

   function Create (arg1 : Obj_T; arg2 : Obj_T) return Obj_T;  -- lv_obj.h:232
   pragma Import (C, Create, "lv_obj_create");

   function Del (arg1 : Obj_T) return lv_res_t;  -- lv_obj.h:239
   pragma Import (C, Del, "lv_obj_del");

   procedure Del (arg1 : Obj_T)  -- lv_obj.h:239
     with Import => True,
     Convention  => C,
     External_Name => "lv_obj_del";

   procedure Clean (arg1 : Obj_T);  -- lv_obj.h:245
   pragma Import (C, Clean, "lv_obj_clean");

   procedure Invalidate (arg1 : Obj_T);  -- lv_obj.h:251
   pragma Import (C, Invalidate, "lv_obj_invalidate");

   procedure Scr_Load (arg1 : Obj_T);  -- lv_obj.h:265
   pragma Import (C, Scr_Load, "lv_scr_load");

   procedure Set_Parent (arg1 : Obj_T; arg2 : Obj_T);  -- lv_obj.h:276
   pragma Import (C, Set_Parent, "lv_obj_set_parent");

   procedure Set_Pos
     (arg1 : Obj_T;
      arg2 : LV.Area.Coord_T;
      arg3 : LV.Area.Coord_T);  -- lv_obj.h:288
   pragma Import (C, Set_Pos, "lv_obj_set_pos");

   procedure Set_X (Self : Obj_T; X : LV.Area.Coord_T);  -- lv_obj.h:295
   pragma Import (C, Set_X, "lv_obj_set_x");

   procedure Set_Y (Self : Obj_T; Y : LV.Area.Coord_T);  -- lv_obj.h:302
   pragma Import (C, Set_Y, "lv_obj_set_y");

   procedure Set_Size
     (Self : Obj_T;
      X    : LV.Area.Coord_T;
      Y    : LV.Area.Coord_T);  -- lv_obj.h:310
   pragma Import (C, Set_Size, "lv_obj_set_size");

   procedure set_width (arg1 : Obj_T; arg2 : LV.Area.Coord_T);  -- lv_obj.h:317
   pragma Import (C, set_width, "lv_obj_set_width");

   procedure set_height (arg1 : Obj_T; arg2 : LV.Area.Coord_T);  -- lv_obj.h:324
   pragma Import (C, set_height, "lv_obj_set_height");

   procedure align
     (arg1 : Obj_T;
      arg2 : Obj_T;
      arg3 : lv_align_t;
      arg4 : LV.Area.Coord_T;
      arg5 : LV.Area.Coord_T);  -- lv_obj.h:334
   pragma Import (C, align, "lv_obj_align");

   procedure set_style (arg1 : Obj_T; arg2 : access LV.Style.Style);  -- lv_obj.h:346
   pragma Import (C, set_style, "lv_obj_set_style");

   procedure refresh_style (arg1 : Obj_T);  -- lv_obj.h:352
   pragma Import (C, refresh_style, "lv_obj_refresh_style");

   procedure report_style_mod (arg1 : access LV.Style.Style);  -- lv_obj.h:359
   pragma Import (C, report_style_mod, "lv_obj_report_style_mod");

   procedure set_hidden (arg1 : Obj_T; arg2 : u_Bool);  -- lv_obj.h:370
   pragma Import (C, set_hidden, "lv_obj_set_hidden");

   procedure set_click (arg1 : Obj_T; arg2 : u_Bool);  -- lv_obj.h:377
   pragma Import (C, set_click, "lv_obj_set_click");

   procedure set_top (arg1 : Obj_T; arg2 : u_Bool);  -- lv_obj.h:385
   pragma Import (C, set_top, "lv_obj_set_top");

   procedure set_drag (arg1 : Obj_T; arg2 : u_Bool);  -- lv_obj.h:392
   pragma Import (C, set_drag, "lv_obj_set_drag");

   procedure set_drag_throw (arg1 : Obj_T; arg2 : u_Bool);  -- lv_obj.h:399
   pragma Import (C, set_drag_throw, "lv_obj_set_drag_throw");

   procedure set_drag_parent (arg1 : Obj_T; arg2 : u_Bool);  -- lv_obj.h:407
   pragma Import (C, set_drag_parent, "lv_obj_set_drag_parent");

   procedure set_editable (arg1 : Obj_T; arg2 : u_Bool);  -- lv_obj.h:415
   pragma Import (C, set_editable, "lv_obj_set_editable");

   procedure set_opa_scale_enable (arg1 : Obj_T; arg2 : u_Bool);  -- lv_obj.h:422
   pragma Import (C, set_opa_scale_enable, "lv_obj_set_opa_scale_enable");

   procedure set_opa_scale (arg1 : Obj_T; arg2 : LV.Color.lv_opa_t);  -- lv_obj.h:429
   pragma Import (C, set_opa_scale, "lv_obj_set_opa_scale");

   procedure set_protect (arg1 : Obj_T; arg2 : uint8_t);  -- lv_obj.h:436
   pragma Import (C, set_protect, "lv_obj_set_protect");

   procedure clear_protect (arg1 : Obj_T; arg2 : uint8_t);  -- lv_obj.h:443
   pragma Import (C, clear_protect, "lv_obj_clear_protect");

   procedure set_signal_func (arg1 : Obj_T; arg2 : lv_signal_func_t);  -- lv_obj.h:451
   pragma Import (C, set_signal_func, "lv_obj_set_signal_func");

   procedure set_design_func (arg1 : Obj_T; arg2 : lv_design_func_t);  -- lv_obj.h:458
   pragma Import (C, set_design_func, "lv_obj_set_design_func");

   function allocate_ext_attr (arg1 : Obj_T; arg2 : uint16_t) return System.Address;  -- lv_obj.h:470
   pragma Import (C, allocate_ext_attr, "lv_obj_allocate_ext_attr");

   procedure refresh_ext_size (arg1 : Obj_T);  -- lv_obj.h:476
   pragma Import (C, refresh_ext_size, "lv_obj_refresh_ext_size");

   procedure set_free_num (arg1 : Obj_T; arg2 : uint32_t);  -- lv_obj.h:485
   pragma Import (C, set_free_num, "lv_obj_set_free_num");

   procedure set_free_ptr (arg1 : Obj_T; arg2 : System.Address);  -- lv_obj.h:495
   pragma Import (C, set_free_ptr, "lv_obj_set_free_ptr");

   procedure animate
     (arg1 : Obj_T;
      arg2 : lv_anim_builtin_t;
      arg3 : uint16_t;
      arg4 : uint16_t;
      arg5 : access procedure (arg1 : Obj_T));  -- lv_obj.h:507
   pragma Import (C, animate, "lv_obj_animate");

   function lv_scr_act return Obj_T;  -- lv_obj.h:522
   pragma Import (C, lv_scr_act, "lv_scr_act");

   function lv_layer_top return Obj_T;  -- lv_obj.h:528
   pragma Import (C, lv_layer_top, "lv_layer_top");

   function lv_layer_sys return Obj_T;  -- lv_obj.h:535
   pragma Import (C, lv_layer_sys, "lv_layer_sys");

   function get_screen (arg1 : Obj_T) return Obj_T;  -- lv_obj.h:542
   pragma Import (C, get_screen, "lv_obj_get_screen");

   function get_parent (arg1 : Obj_T) return Obj_T;  -- lv_obj.h:553
   pragma Import (C, get_parent, "lv_obj_get_parent");

   function get_child (arg1 : Obj_T; arg2 : Obj_T) return Obj_T;  -- lv_obj.h:562
   pragma Import (C, get_child, "lv_obj_get_child");

   function get_child_back (arg1 : Obj_T; arg2 : Obj_T) return Obj_T;  -- lv_obj.h:571
   pragma Import (C, get_child_back, "lv_obj_get_child_back");

   function count_children (arg1 : Obj_T) return uint16_t;  -- lv_obj.h:578
   pragma Import (C, count_children, "lv_obj_count_children");

   procedure get_coords (arg1 : Obj_T; arg2 : access LV.Area.Area_T);  -- lv_obj.h:589
   pragma Import (C, get_coords, "lv_obj_get_coords");

   function get_x (arg1 : Obj_T) return LV.Area.Coord_T;  -- lv_obj.h:596
   pragma Import (C, get_x, "lv_obj_get_x");

   function get_y (arg1 : Obj_T) return LV.Area.Coord_T;  -- lv_obj.h:603
   pragma Import (C, get_y, "lv_obj_get_y");

   function get_width (arg1 : Obj_T) return LV.Area.Coord_T;  -- lv_obj.h:610
   pragma Import (C, get_width, "lv_obj_get_width");

   function get_height (arg1 : Obj_T) return LV.Area.Coord_T;  -- lv_obj.h:617
   pragma Import (C, get_height, "lv_obj_get_height");

   function get_ext_size (arg1 : Obj_T) return LV.Area.Coord_T;  -- lv_obj.h:624
   pragma Import (C, get_ext_size, "lv_obj_get_ext_size");

   function get_style (arg1 : Obj_T) return LV.Style.Style;  -- lv_obj.h:635
   pragma Import (C, get_style, "lv_obj_get_style");

   function get_hidden (arg1 : Obj_T) return u_Bool;  -- lv_obj.h:646
   pragma Import (C, get_hidden, "lv_obj_get_hidden");

   function get_click (arg1 : Obj_T) return u_Bool;  -- lv_obj.h:653
   pragma Import (C, get_click, "lv_obj_get_click");

   function get_top (arg1 : Obj_T) return u_Bool;  -- lv_obj.h:660
   pragma Import (C, get_top, "lv_obj_get_top");

   function get_drag (arg1 : Obj_T) return u_Bool;  -- lv_obj.h:667
   pragma Import (C, get_drag, "lv_obj_get_drag");

   function get_drag_throw (arg1 : Obj_T) return u_Bool;  -- lv_obj.h:674
   pragma Import (C, get_drag_throw, "lv_obj_get_drag_throw");

   function get_drag_parent (arg1 : Obj_T) return u_Bool;  -- lv_obj.h:681
   pragma Import (C, get_drag_parent, "lv_obj_get_drag_parent");

   function get_opa_scale (arg1 : Obj_T) return LV.Color.lv_opa_t;  -- lv_obj.h:688
   pragma Import (C, get_opa_scale, "lv_obj_get_opa_scale");

   function get_protect (arg1 : Obj_T) return uint8_t;  -- lv_obj.h:695
   pragma Import (C, get_protect, "lv_obj_get_protect");

   function is_protected (arg1 : Obj_T; arg2 : uint8_t) return u_Bool;  -- lv_obj.h:703
   pragma Import (C, is_protected, "lv_obj_is_protected");

   function get_signal_func (arg1 : Obj_T) return lv_signal_func_t;  -- lv_obj.h:710
   pragma Import (C, get_signal_func, "lv_obj_get_signal_func");

   function get_design_func (arg1 : Obj_T) return lv_design_func_t;  -- lv_obj.h:717
   pragma Import (C, get_design_func, "lv_obj_get_design_func");

   function get_ext_attr (arg1 : Obj_T) return System.Address;  -- lv_obj.h:729
   pragma Import (C, get_ext_attr, "lv_obj_get_ext_attr");

   procedure get_type (arg1 : Obj_T; arg2 : lv_obj_type_t);  -- lv_obj.h:737
   pragma Import (C, get_type, "lv_obj_get_type");

   function get_free_num (arg1 : Obj_T) return uint32_t;  -- lv_obj.h:745
   pragma Import (C, get_free_num, "lv_obj_get_free_num");

   function get_free_ptr (arg1 : Obj_T) return System.Address;  -- lv_obj.h:754
   pragma Import (C, get_free_ptr, "lv_obj_get_free_ptr");

   function get_group (arg1 : Obj_T) return System.Address;  -- lv_obj.h:763
   pragma Import (C, get_group, "lv_obj_get_group");

   function is_focused (arg1 : Obj_T) return u_Bool;  -- lv_obj.h:771
   pragma Import (C, is_focused, "lv_obj_is_focused");

--  private
--
--     type u_lv_obj_t is record
--        par : access u_lv_obj_t;  -- lv_obj.h:117
--        child_ll : aliased lv_ll_h.lv_ll_t;  -- lv_obj.h:118
--        coords : aliased LV.Area.lv_area_t;  -- lv_obj.h:120
--        signal_func : lv_signal_func_t;  -- lv_obj.h:122
--        design_func : lv_design_func_t;  -- lv_obj.h:123
--        ext_attr : System.Address;  -- lv_obj.h:125
--        style_p : access LV.Style.lv_style_t;  -- lv_obj.h:126
--        free_ptr : System.Address;  -- lv_obj.h:129
--        group_p : System.Address;  -- lv_obj.h:133
--        click : Extensions.Unsigned_1;  -- lv_obj.h:136
--        drag : Extensions.Unsigned_1;  -- lv_obj.h:137
--        drag_throw : Extensions.Unsigned_1;  -- lv_obj.h:138
--        drag_parent : Extensions.Unsigned_1;  -- lv_obj.h:139
--        hidden : Extensions.Unsigned_1;  -- lv_obj.h:140
--        top : Extensions.Unsigned_1;  -- lv_obj.h:141
--        opa_scale_en : Extensions.Unsigned_1;  -- lv_obj.h:142
--        protect : aliased uint8_t;  -- lv_obj.h:143
--        opa_scale : aliased LV.Color.lv_opa_t;  -- lv_obj.h:144
--        ext_size : aliased LV.Area.Coord_T;  -- lv_obj.h:146
--        free_num : aliased uint32_t;  -- lv_obj.h:149
--     end record;
--     pragma Convention (C_Pass_By_Copy, u_lv_obj_t);
--     pragma Pack (u_lv_obj_t);  -- lv_obj.h:115
--
--     subtype lv_obj_t is u_lv_obj_t;  -- lv_obj.h:151

end LV.Objx;
