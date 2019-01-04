pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lv.Style;
with Interfaces.C.Extensions;

with Lv.Objx.Cont;

package Lv.Objx.Btn is

   subtype Instance is Obj_T;

   type State_T is
     (State_Rel,
      State_Pr,
      State_Tgl_Rel,
      State_Tgl_Pr,
      State_Ina,
      State_Num) with
        Size => 8;

   for State_T use
     (State_Rel     => 0,
      State_Pr      => 1,
      State_Tgl_Rel => 2,
      State_Tgl_Pr  => 3,
      State_Ina     => 4,
      State_Num     => 5);

   type Action_T is
     (Action_Click,
      Action_Pr,
      Action_Long_Pr,
      Action_Long_Pr_Repeat,
      Action_Num) with
        Size => 8;

   for Action_T use
     (Action_Click          => 0,
      Action_Pr             => 1,
      Action_Long_Pr        => 2,
      Action_Long_Pr_Repeat => 3,
      Action_Num            => 4);

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_btn_create");

   procedure Set_Toggle (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Set_Toggle, "lv_btn_set_toggle");

   procedure Set_State (Self : Instance; Arg2 : State_T);
   pragma Import (C, Set_State, "lv_btn_set_state");

   procedure Toggle (Self : Instance);
   pragma Import (C, Toggle, "lv_btn_toggle");

   procedure Set_Action (Self : Instance; Arg2 : Action_T; Arg3 : Lv_Action_T);
   pragma Import (C, Set_Action, "lv_btn_set_action");

   procedure Set_Layout (Self : Instance; Layout : Lv.Objx.Cont.Layout_T);
   pragma Import (C, Set_Layout, "lv_btn_set_layout_inline");

   procedure Set_Fit (Self : Instance; Hor_En : U_Bool; Ver_En : U_Bool);
   pragma Import (C, Set_Fit, "lv_btn_set_fit_inline");

   procedure Set_Ink_In_Time (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Ink_In_Time, "lv_btn_set_ink_in_time");

   procedure Set_Ink_Wait_Time (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Ink_Wait_Time, "lv_btn_set_ink_wait_time");

   procedure Set_Ink_Out_Time (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Ink_Out_Time, "lv_btn_set_ink_out_time");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_btn_set_style");

   function Get_State (Self : Instance) return State_T;
   pragma Import (C, Get_State, "lv_btn_get_state");

   function Get_Toggle (Self : Instance) return U_Bool;
   pragma Import (C, Get_Toggle, "lv_btn_get_toggle");

   function Get_Action (Self : Instance; Arg2 : Action_T) return Lv_Action_T;
   pragma Import (C, Get_Action, "lv_btn_get_action");

   function Get_Layout (Self : Instance) return Lv.Objx.Cont.Layout_T;
   pragma Import (C, Get_Layout, "lv_btn_get_layout_inline");

   function Get_Hor_Fit (Self : Instance) return U_Bool;
   pragma Import (C, Get_Hor_Fit, "lv_btn_get_hor_fit_inline");

   function Get_Ver_Fit (Self : Instance) return U_Bool;
   pragma Import (C, Get_Ver_Fit, "lv_btn_get_ver_fit_inline");

   function Get_Ink_In_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Ink_In_Time, "lv_btn_get_ink_in_time");

   function Get_Ink_Wait_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Ink_Wait_Time, "lv_btn_get_ink_wait_time");

   function Get_Ink_Out_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Ink_Out_Time, "lv_btn_get_ink_out_time");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_btn_get_style");

end Lv.Objx.Btn;
