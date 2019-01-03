with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Lv.Style;

package Lv.Objx.Checkbox is

   subtype Instance is Obj_T;

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_cb_create");

   procedure Set_Text (Self : Instance; Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Text, "lv_cb_set_text");

   procedure Set_Checked (Self : Instance; Checked : U_Bool);
   pragma Import (C, Set_Checked, "lv_cb_set_checked_inline");

   procedure Set_Inactive (Self : Instance);
   pragma Import (C, Set_Inactive, "lv_cb_set_inactive_inline");

   procedure Set_Action (Self : Instance; Action : Lv_Action_T);
   pragma Import (C, Set_Action, "lv_cb_set_action_inline");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_cb_set_style");

   function Get_Text (Self : Instance) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Text, "lv_cb_get_text");

   function Is_Checked (Self : Instance) return U_Bool;
   pragma Import (C, Is_Checked, "lv_cb_is_checked_inline");

   function Get_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Action, "lv_cb_get_action_inline");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_cb_get_style");

end Lv.Objx.Checkbox;
