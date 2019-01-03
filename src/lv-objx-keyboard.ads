pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lv.Objx.Btnm;
with Interfaces.C.Extensions;
with System;
with Lv.Style;

with Lv.Objx.Textarea;

package Lv.Objx.Keyboard is

   subtype Instance is Obj_T;

   type Lv_Kb_Mode_T is (Mode_Text, Mode_Num) with
        Size => 8;

   for Lv_Kb_Mode_T use (Mode_Text => 0, Mode_Num => 1);

   type Lv_Kb_Style_T is
     (Style_Bg,
      Style_Btn_Rel,
      Style_Btn_Pr,
      Style_Btn_Tgl_Rel,
      Style_Btn_Tgl_Pr,
      Style_Btn_Ina) with
        Size => 8;

   for Lv_Kb_Style_T use
     (Style_Bg          => 0,
      Style_Btn_Rel     => 1,
      Style_Btn_Pr      => 2,
      Style_Btn_Tgl_Rel => 3,
      Style_Btn_Tgl_Pr  => 4,
      Style_Btn_Ina     => 5);

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_kb_create");

   procedure Set_Textarea (Self : Instance; Ta : Textarea.Textarea);
   pragma Import (C, Set_Textarea, "lv_kb_set_ta");

   procedure Set_Mode (Self : Instance; Arg2 : Lv_Kb_Mode_T);
   pragma Import (C, Set_Mode, "lv_kb_set_mode");

   procedure Set_Cursor_Manage (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Set_Cursor_Manage, "lv_kb_set_cursor_manage");

   procedure Set_Ok_Action (Self : Instance; Arg2 : Lv_Action_T);
   pragma Import (C, Set_Ok_Action, "lv_kb_set_ok_action");

   procedure Set_Hide_Action (Self : Instance; Arg2 : Lv_Action_T);
   pragma Import (C, Set_Hide_Action, "lv_kb_set_hide_action");

   procedure Set_Map (Self : Instance; Map : System.Address);
   pragma Import (C, Set_Map, "lv_kb_set_map_inline");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Lv_Kb_Style_T;
      Arg3 : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_kb_set_style");

   function Get_Textarea (Self : Instance) return Textarea.Textarea;
   pragma Import (C, Get_Textarea, "lv_kb_get_ta");

   function Get_Mode (Self : Instance) return Lv_Kb_Mode_T;
   pragma Import (C, Get_Mode, "lv_kb_get_mode");

   function Get_Cursor_Manage (Self : Instance) return U_Bool;
   pragma Import (C, Get_Cursor_Manage, "lv_kb_get_cursor_manage");

   function Get_Ok_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Ok_Action, "lv_kb_get_ok_action");

   function Get_Hide_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Hide_Action, "lv_kb_get_hide_action");

   function Get_Style
     (Self : Instance;
      Arg2 : Lv_Kb_Style_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_kb_get_style");

end Lv.Objx.Keyboard;
