pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Lv.Style;
with System;
with Interfaces.C.Extensions;

package Lv.Objx.Btnm is

   subtype Instance is Obj_T;

   Lv_Btnm_Ctrl_Code           : constant := 16#80#;
   Lv_Btnm_Ctrl_Mask           : constant := 16#C0#;
   Lv_Btnm_Width_Mask          : constant := 16#07#;
   Lv_Btnm_Hide_Mask           : constant := 16#08#;
   Lv_Btnm_Repeat_Disable_Mask : constant := 16#10#;
   Lv_Btnm_Inactive_Mask       : constant := 16#20#;

   Lv_Btnm_Pr_None : constant := 16#FFFF#;

   type Action_T is access function
     (Arg1 : access Obj_T;
      Arg2 : Interfaces.C.Strings.chars_ptr) return Lv_Res_T;
   pragma Convention (C, Action_T);

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_btnm_create");

   procedure Set_Map (Self : Instance; Arg2 : System.Address);
   pragma Import (C, Set_Map, "lv_btnm_set_map");

   procedure Set_Action (Self : Instance; Arg2 : Action_T);
   pragma Import (C, Set_Action, "lv_btnm_set_action");

   procedure Set_Toggle (Self : Instance; Arg2 : U_Bool; Arg3 : Uint16_T);
   pragma Import (C, Set_Toggle, "lv_btnm_set_toggle");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_btnm_set_style");

   function Get_Map (Self : Instance) return System.Address;
   pragma Import (C, Get_Map, "lv_btnm_get_map");

   function Get_Action (Self : Instance) return Action_T;
   pragma Import (C, Get_Action, "lv_btnm_get_action");

   function Get_Toggled (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Toggled, "lv_btnm_get_toggled");

   function Get_Style (Self : Instance; Arg2 : Style_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_btnm_get_style");

end Lv.Objx.Btnm;
