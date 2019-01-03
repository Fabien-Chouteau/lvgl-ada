pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Lv.Objx.Btnm;
with Interfaces.C.Strings;
with Lv.Style;

package Lv.Objx.Mbox is

   subtype Instance is Obj_T;

   subtype Style_T is Uint8_T;

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_mbox_create");

   procedure Add_Btns
     (Self    : Instance;
      Btn_Map : System.Address;
      Action  : Lv.Objx.Btnm.Action_T);
   pragma Import (C, Add_Btns, "lv_mbox_add_btns");

   procedure Set_Text (Self : Instance; Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Text, "lv_mbox_set_text");

   procedure Set_Action (Self : Instance; Arg2 : Lv.Objx.Btnm.Action_T);
   pragma Import (C, Set_Action, "lv_mbox_set_action");

   procedure Set_Anim_Time (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Anim_Time, "lv_mbox_set_anim_time");

   procedure Start_Auto_Close (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Start_Auto_Close, "lv_mbox_start_auto_close");

   procedure Stop_Auto_Close (Self : Instance);
   pragma Import (C, Stop_Auto_Close, "lv_mbox_stop_auto_close");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_mbox_set_style");

   function Get_Text (Self : Instance) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Text, "lv_mbox_get_text");

   function Get_From_Btn (Button : Obj_T) return Instance;
   pragma Import (C, Get_From_Btn, "lv_mbox_get_from_btn");

   function Get_Anim_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Anim_Time, "lv_mbox_get_anim_time");

   function Get_Style (Self : Instance; Arg2 : Style_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_mbox_get_style");

end Lv.Objx.Mbox;
