with System;
with Lv.Area;
with Interfaces.C.Strings;
with Lv.Style;
with Lv.Color; use Lv.Color;
with Interfaces.C.Extensions;

package Lv.Objx is

   type Obj_T is new System.Address;

   No_Obj : constant Obj_T := Obj_T (System.Null_Address);

   Anim_In       : constant := 16#00#;
   Anim_Out      : constant := 16#80#;
   Anim_Dir_Mask : constant := 16#80#;

   Lv_Max_Ancestor_Num : constant := 8;

   Res_Inv : constant := 0;
   Res_Ok  : constant := 1;

   type Align_T is
     (Align_Center,
      Align_In_Top_Left,
      Align_In_Top_Mid,
      Align_In_Top_Right,
      Align_In_Bottom_Left,
      Align_In_Bottom_Mid,
      Align_In_Bottom_Right,
      Align_In_Left_Mid,
      Align_In_Right_Mid,
      Align_Out_Top_Left,
      Align_Out_Top_Mid,
      Align_Out_Top_Right,
      Align_Out_Bottom_Left,
      Align_Out_Bottom_Mid,
      Align_Out_Bottom_Right,
      Align_Out_Left_Top,
      Align_Out_Left_Mid,
      Align_Out_Left_Bottom,
      Align_Out_Right_Top,
      Align_Out_Right_Mid,
      Align_Out_Right_Bottom)
   with Size => 8;

   for Align_T use
     (Align_Center           => 0,
      Align_In_Top_Left      => 1,
      Align_In_Top_Mid       => 2,
      Align_In_Top_Right     => 3,
      Align_In_Bottom_Left   => 4,
      Align_In_Bottom_Mid    => 5,
      Align_In_Bottom_Right  => 6,
      Align_In_Left_Mid      => 7,
      Align_In_Right_Mid     => 8,
      Align_Out_Top_Left     => 9,
      Align_Out_Top_Mid      => 10,
      Align_Out_Top_Right    => 11,
      Align_Out_Bottom_Left  => 12,
      Align_Out_Bottom_Mid   => 13,
      Align_Out_Bottom_Right => 14,
      Align_Out_Left_Top     => 15,
      Align_Out_Left_Mid     => 16,
      Align_Out_Left_Bottom  => 17,
      Align_Out_Right_Top    => 18,
      Align_Out_Right_Mid    => 19,
      Align_Out_Right_Bottom => 20);

   subtype Lv_Design_Mode_T is Uint8_T;

   type Lv_Design_Func_T is access function
     (Arg1 : System.Address;
      Arg2 : access constant Lv.Area.Area_T;
      Arg3 : Lv_Design_Mode_T) return U_Bool;
   pragma Convention (C, Lv_Design_Func_T);

   subtype Lv_Res_T is Uint8_T;

   subtype Lv_Signal_T is Uint8_T;

   type Lv_Signal_Func_T is access function
     (Arg1 : System.Address;
      Arg2 : Lv_Signal_T;
      Arg3 : System.Address) return Lv_Res_T;
   pragma Convention (C, Lv_Signal_Func_T);

   type Lv_Action_T is access function (Arg1 : Obj_T) return Lv_Res_T;
   pragma Convention (C, Lv_Action_T);

   subtype Lv_Protect_T is Uint8_T;

   type Lv_Obj_Type_T_C_Type_Array is
     array (0 .. 7) of Interfaces.C.Strings.chars_ptr;
   type Lv_Obj_Type_T is record
      C_Type : Lv_Obj_Type_T_C_Type_Array;
   end record;
   pragma Convention (C_Pass_By_Copy, Lv_Obj_Type_T);

   subtype Lv_Anim_Builtin_T is Uint8_T;
   Anim_None         : constant Lv_Anim_Builtin_T := 0;
   Anim_Float_Top    : constant Lv_Anim_Builtin_T := 1;
   Anim_Float_Left   : constant Lv_Anim_Builtin_T := 2;
   Anim_Float_Bottom : constant Lv_Anim_Builtin_T := 3;
   Anim_Float_Right  : constant Lv_Anim_Builtin_T := 4;
   Anim_Grow_H       : constant Lv_Anim_Builtin_T := 5;
   Anim_Grow_V       : constant Lv_Anim_Builtin_T := 6;

   function Create (Arg1 : Obj_T; Arg2 : Obj_T) return Obj_T;
   pragma Import (C, Create, "lv_obj_create");

   function Del (Arg1 : Obj_T) return Lv_Res_T;
   pragma Import (C, Del, "lv_obj_del");

   procedure Del (Arg1 : Obj_T) with
      Import        => True,
      Convention    => C,
      External_Name => "lv_obj_del";

   procedure Clean (Arg1 : Obj_T);
   pragma Import (C, Clean, "lv_obj_clean");

   procedure Invalidate (Arg1 : Obj_T);
   pragma Import (C, Invalidate, "lv_obj_invalidate");

   procedure Scr_Load (Arg1 : Obj_T);
   pragma Import (C, Scr_Load, "lv_scr_load");

   procedure Set_Parent (Arg1 : Obj_T; Arg2 : Obj_T);
   pragma Import (C, Set_Parent, "lv_obj_set_parent");

   procedure Set_Pos
     (Arg1 : Obj_T;
      Arg2 : Lv.Area.Coord_T;
      Arg3 : Lv.Area.Coord_T);
   pragma Import (C, Set_Pos, "lv_obj_set_pos");

   procedure Set_X (Self : Obj_T; X : Lv.Area.Coord_T);
   pragma Import (C, Set_X, "lv_obj_set_x");

   procedure Set_Y (Self : Obj_T; Y : Lv.Area.Coord_T);
   pragma Import (C, Set_Y, "lv_obj_set_y");

   procedure Set_Size (Self : Obj_T; X : Lv.Area.Coord_T; Y : Lv.Area.Coord_T);
   pragma Import (C, Set_Size, "lv_obj_set_size");

   procedure Set_Width (Arg1 : Obj_T; Arg2 : Lv.Area.Coord_T);
   pragma Import (C, Set_Width, "lv_obj_set_width");

   procedure Set_Height (Arg1 : Obj_T; Arg2 : Lv.Area.Coord_T);
   pragma Import (C, Set_Height, "lv_obj_set_height");

   procedure Align
     (Arg1 : Obj_T;
      Arg2 : Obj_T;
      Arg3 : Align_T;
      Arg4 : Lv.Area.Coord_T;
      Arg5 : Lv.Area.Coord_T);
   pragma Import (C, Align, "lv_obj_align");

   procedure Set_Style (Arg1 : Obj_T; Arg2 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_obj_set_style");

   procedure Refresh_Style (Arg1 : Obj_T);
   pragma Import (C, Refresh_Style, "lv_obj_refresh_style");

   procedure Report_Style_Mod (Arg1 : access Lv.Style.Style);
   pragma Import (C, Report_Style_Mod, "lv_obj_report_style_mod");

   procedure Set_Hidden (Arg1 : Obj_T; Arg2 : U_Bool);
   pragma Import (C, Set_Hidden, "lv_obj_set_hidden");

   procedure Set_Click (Arg1 : Obj_T; Arg2 : U_Bool);
   pragma Import (C, Set_Click, "lv_obj_set_click");

   procedure Set_Top (Arg1 : Obj_T; Arg2 : U_Bool);
   pragma Import (C, Set_Top, "lv_obj_set_top");

   procedure Set_Drag (Arg1 : Obj_T; Arg2 : U_Bool);
   pragma Import (C, Set_Drag, "lv_obj_set_drag");

   procedure Set_Drag_Throw (Arg1 : Obj_T; Arg2 : U_Bool);
   pragma Import (C, Set_Drag_Throw, "lv_obj_set_drag_throw");

   procedure Set_Drag_Parent (Arg1 : Obj_T; Arg2 : U_Bool);
   pragma Import (C, Set_Drag_Parent, "lv_obj_set_drag_parent");

   procedure Set_Editable (Arg1 : Obj_T; Arg2 : U_Bool);
   pragma Import (C, Set_Editable, "lv_obj_set_editable");

   procedure Set_Opa_Scale_Enable (Arg1 : Obj_T; Arg2 : U_Bool);
   pragma Import (C, Set_Opa_Scale_Enable, "lv_obj_set_opa_scale_enable");

   procedure Set_Opa_Scale (Arg1 : Obj_T; Arg2 : Lv.Color.Opa_T);
   pragma Import (C, Set_Opa_Scale, "lv_obj_set_opa_scale");

   procedure Set_Protect (Arg1 : Obj_T; Arg2 : Uint8_T);
   pragma Import (C, Set_Protect, "lv_obj_set_protect");

   procedure Clear_Protect (Arg1 : Obj_T; Arg2 : Uint8_T);
   pragma Import (C, Clear_Protect, "lv_obj_clear_protect");

   procedure Set_Signal_Func (Arg1 : Obj_T; Arg2 : Lv_Signal_Func_T);
   pragma Import (C, Set_Signal_Func, "lv_obj_set_signal_func");

   procedure Set_Design_Func (Arg1 : Obj_T; Arg2 : Lv_Design_Func_T);
   pragma Import (C, Set_Design_Func, "lv_obj_set_design_func");

   function Allocate_Ext_Attr
     (Arg1 : Obj_T;
      Arg2 : Uint16_T) return System.Address;
   pragma Import (C, Allocate_Ext_Attr, "lv_obj_allocate_ext_attr");

   procedure Refresh_Ext_Size (Arg1 : Obj_T);
   pragma Import (C, Refresh_Ext_Size, "lv_obj_refresh_ext_size");

   procedure Set_Free_Num (Arg1 : Obj_T; Arg2 : Uint32_T);
   pragma Import (C, Set_Free_Num, "lv_obj_set_free_num");

   procedure Set_Free_Ptr (Arg1 : Obj_T; Arg2 : System.Address);
   pragma Import (C, Set_Free_Ptr, "lv_obj_set_free_ptr");

   procedure Animate
     (Arg1 : Obj_T;
      Arg2 : Lv_Anim_Builtin_T;
      Arg3 : Uint16_T;
      Arg4 : Uint16_T;
      Arg5 : access procedure (Arg1 : Obj_T));
   pragma Import (C, Animate, "lv_obj_animate");

   function Lv_Scr_Act return Obj_T;
   pragma Import (C, Lv_Scr_Act, "lv_scr_act");

   function Lv_Layer_Top return Obj_T;
   pragma Import (C, Lv_Layer_Top, "lv_layer_top");

   function Lv_Layer_Sys return Obj_T;
   pragma Import (C, Lv_Layer_Sys, "lv_layer_sys");

   function Get_Screen (Arg1 : Obj_T) return Obj_T;
   pragma Import (C, Get_Screen, "lv_obj_get_screen");

   function Get_Parent (Arg1 : Obj_T) return Obj_T;
   pragma Import (C, Get_Parent, "lv_obj_get_parent");

   function Get_Child (Arg1 : Obj_T; Arg2 : Obj_T) return Obj_T;
   pragma Import (C, Get_Child, "lv_obj_get_child");

   function Get_Child_Back (Arg1 : Obj_T; Arg2 : Obj_T) return Obj_T;
   pragma Import (C, Get_Child_Back, "lv_obj_get_child_back");

   function Count_Children (Arg1 : Obj_T) return Uint16_T;
   pragma Import (C, Count_Children, "lv_obj_count_children");

   procedure Get_Coords (Arg1 : Obj_T; Arg2 : access Lv.Area.Area_T);
   pragma Import (C, Get_Coords, "lv_obj_get_coords");

   function Get_X (Arg1 : Obj_T) return Lv.Area.Coord_T;
   pragma Import (C, Get_X, "lv_obj_get_x");

   function Get_Y (Arg1 : Obj_T) return Lv.Area.Coord_T;
   pragma Import (C, Get_Y, "lv_obj_get_y");

   function Get_Width (Arg1 : Obj_T) return Lv.Area.Coord_T;
   pragma Import (C, Get_Width, "lv_obj_get_width");

   function Get_Height (Arg1 : Obj_T) return Lv.Area.Coord_T;
   pragma Import (C, Get_Height, "lv_obj_get_height");

   function Get_Ext_Size (Arg1 : Obj_T) return Lv.Area.Coord_T;
   pragma Import (C, Get_Ext_Size, "lv_obj_get_ext_size");

   function Get_Style (Arg1 : Obj_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_obj_get_style");

   function Get_Hidden (Arg1 : Obj_T) return U_Bool;
   pragma Import (C, Get_Hidden, "lv_obj_get_hidden");

   function Get_Click (Arg1 : Obj_T) return U_Bool;
   pragma Import (C, Get_Click, "lv_obj_get_click");

   function Get_Top (Arg1 : Obj_T) return U_Bool;
   pragma Import (C, Get_Top, "lv_obj_get_top");

   function Get_Drag (Arg1 : Obj_T) return U_Bool;
   pragma Import (C, Get_Drag, "lv_obj_get_drag");

   function Get_Drag_Throw (Arg1 : Obj_T) return U_Bool;
   pragma Import (C, Get_Drag_Throw, "lv_obj_get_drag_throw");

   function Get_Drag_Parent (Arg1 : Obj_T) return U_Bool;
   pragma Import (C, Get_Drag_Parent, "lv_obj_get_drag_parent");

   function Get_Opa_Scale (Arg1 : Obj_T) return Lv.Color.Opa_T;
   pragma Import (C, Get_Opa_Scale, "lv_obj_get_opa_scale");

   function Get_Protect (Arg1 : Obj_T) return Uint8_T;
   pragma Import (C, Get_Protect, "lv_obj_get_protect");

   function Is_Protected (Arg1 : Obj_T; Arg2 : Uint8_T) return U_Bool;
   pragma Import (C, Is_Protected, "lv_obj_is_protected");

   function Get_Signal_Func (Arg1 : Obj_T) return Lv_Signal_Func_T;
   pragma Import (C, Get_Signal_Func, "lv_obj_get_signal_func");

   function Get_Design_Func (Arg1 : Obj_T) return Lv_Design_Func_T;
   pragma Import (C, Get_Design_Func, "lv_obj_get_design_func");

   function Get_Ext_Attr (Arg1 : Obj_T) return System.Address;
   pragma Import (C, Get_Ext_Attr, "lv_obj_get_ext_attr");

   procedure Get_Type (Arg1 : Obj_T; Arg2 : Lv_Obj_Type_T);
   pragma Import (C, Get_Type, "lv_obj_get_type");

   function Get_Free_Num (Arg1 : Obj_T) return Uint32_T;
   pragma Import (C, Get_Free_Num, "lv_obj_get_free_num");

   function Get_Free_Ptr (Arg1 : Obj_T) return System.Address;
   pragma Import (C, Get_Free_Ptr, "lv_obj_get_free_ptr");

   function Get_Group (Arg1 : Obj_T) return System.Address;
   pragma Import (C, Get_Group, "lv_obj_get_group");

   function Is_Focused (Arg1 : Obj_T) return U_Bool;
   pragma Import (C, Is_Focused, "lv_obj_is_focused");

end Lv.Objx;
