pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

with Lv.Style;

package Lv.Objx.Switch is

   subtype Instance is Obj_T;

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_sw_create");

   procedure On (Self : Instance);
   pragma Import (C, On, "lv_sw_on");

   procedure Off (Self : Instance);
   pragma Import (C, Off, "lv_sw_off");

   procedure Set_Action (Self : Instance; Action : Lv_Action_T);
   pragma Import (C, Set_Action, "lv_sw_set_action_inline");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_sw_set_style");

   function Get_State (Self : Instance) return U_Bool;
   pragma Import (C, Get_State, "lv_sw_get_state_inline");

   function Get_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Action, "lv_sw_get_action_inline");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_sw_get_style");

end Lv.Objx.Switch;
