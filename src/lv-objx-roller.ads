with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Lv.Style;

package Lv.Objx.Roller is

   subtype Instance is Obj_T;

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_roller_create");

   procedure Set_Options
     (Self    : Instance;
      Options : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Options, "lv_roller_set_options_inline");

   procedure Set_Selected (Self : Instance; Set_Opt : Uint16_T; Anim_En : U_Bool);
   pragma Import (C, Set_Selected, "lv_roller_set_selected");

   procedure Set_Action (Self : Instance; Action : Lv_Action_T);
   pragma Import (C, Set_Action, "lv_roller_set_action_inline");

   procedure Set_Visible_Row_Count (Self : Instance; Arg2 : Uint8_T);
   pragma Import (C, Set_Visible_Row_Count, "lv_roller_set_visible_row_count");

   procedure Set_Hor_Fit (Self : Instance; Fit_En : U_Bool);
   pragma Import (C, Set_Hor_Fit, "lv_roller_set_hor_fit_inline");

   procedure Set_Anim_Time (Self : Instance; Anim_Time : Uint16_T);
   pragma Import (C, Set_Anim_Time, "lv_roller_set_anim_time_inline");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_roller_set_style");

   function Get_Options
     (Self : Instance) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Options, "lv_roller_get_options_inline");

   function Get_Selected (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Selected, "lv_roller_get_selected_inline");

   procedure Get_Selected_Str
     (Self : Instance;
      Buf  : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Get_Selected_Str, "lv_roller_get_selected_str_inline");

   function Get_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Action, "lv_roller_get_action_inline");

   function Get_Anim_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Anim_Time, "lv_roller_get_anim_time_inline");

   function Get_Hor_Fit (Self : Instance) return U_Bool;
   pragma Import (C, Get_Hor_Fit, "lv_roller_get_hor_fit");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_roller_get_style");

end Lv.Objx.Roller;
