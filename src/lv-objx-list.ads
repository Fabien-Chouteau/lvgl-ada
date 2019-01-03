pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lv.Style;
with System;
with Interfaces.C.Strings;

with Lv.Objx.Label;
with Lv.Objx.Page;
with Lv.Objx.Btn;

package Lv.Objx.List is

   subtype Instance is Obj_T;

   subtype Lv_List_Style_T is Uint8_T;

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_list_create");

   procedure Clean (Self : Instance);
   pragma Import (C, Clean, "lv_list_clean");

   function Add
     (Self : Instance;
      Arg2 : System.Address;
      Arg3 : Interfaces.C.Strings.chars_ptr;
      Arg4 : Lv_Action_T) return Btn.Instance;
   pragma Import (C, Add, "lv_list_add");

   procedure Set_Btn_Selected (Self : Instance; Arg2 : Obj_T);
   pragma Import (C, Set_Btn_Selected, "lv_list_set_btn_selected");

   procedure Set_Anim_Time (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Anim_Time, "lv_list_set_anim_time");

   procedure Set_Sb_Mode (Self : Instance; Mode : Lv.Objx.Page.Lv_Sb_Mode_T);
   pragma Import (C, Set_Sb_Mode, "lv_list_set_sb_mode_inline");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Lv_List_Style_T;
      Arg3 : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_list_set_style");

   function Get_Btn_Text
     (Self : Instance) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Btn_Text, "lv_list_get_btn_text");

   function Get_Btn_Label (Self : Instance) return Label.Instance;
   pragma Import (C, Get_Btn_Label, "lv_list_get_btn_label");

   function Get_Btn_Img (Self : Instance) return Obj_T;
   pragma Import (C, Get_Btn_Img, "lv_list_get_btn_img");

   function Get_Prev_Btn (Self : Instance; Arg2 : Obj_T) return Obj_T;
   pragma Import (C, Get_Prev_Btn, "lv_list_get_prev_btn");

   function Get_Next_Btn (Self : Instance; Arg2 : Obj_T) return Obj_T;
   pragma Import (C, Get_Next_Btn, "lv_list_get_next_btn");

   function Get_Btn_Selected (Self : Instance) return Obj_T;
   pragma Import (C, Get_Btn_Selected, "lv_list_get_btn_selected");

   function Get_Anim_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Anim_Time, "lv_list_get_anim_time");

   function Get_Sb_Mode (Self : Instance) return Lv.Objx.Page.Lv_Sb_Mode_T;
   pragma Import (C, Get_Sb_Mode, "lv_list_get_sb_mode_inline");

   function Get_Style
     (Self : Instance;
      Arg2 : Lv_List_Style_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_list_get_style");

   procedure Up (Self : Instance);
   pragma Import (C, Up, "lv_list_up");

   procedure Down (Self : Instance);
   pragma Import (C, Down, "lv_list_down");

   procedure Focus (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Focus, "lv_list_focus");

end Lv.Objx.List;
