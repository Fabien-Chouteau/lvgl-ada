pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Lv.Area;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with Lv.Style;

package Lv.Objx.Tabview is

   subtype Instance is Obj_T;

   type Lv_Tabview_Action_T is access function
     (Arg1 : access Obj_T;
      Arg2 : Uint16_T) return Res_T;
   pragma Convention (C, Lv_Tabview_Action_T);

   subtype Lv_Tabview_Btns_Pos_T is Uint8_T;

   type Lv_Tabview_Style_T is
     (Style_Bg,
      Style_Indic,
      Style_Btn_Bg,
      Style_Btn_Rel,
      Style_Btn_Pr,
      Style_Btn_Tgl_Rel,
      Style_Btn_Tgl_Pr) with
        Size => 8;

   for Lv_Tabview_Style_T use
     (Style_Bg          => 0,
      Style_Indic       => 1,
      Style_Btn_Bg      => 2,
      Style_Btn_Rel     => 3,
      Style_Btn_Pr      => 4,
      Style_Btn_Tgl_Rel => 5,
      Style_Btn_Tgl_Pr  => 6);

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_tabview_create");

   procedure Clean (Self : Instance);
   pragma Import (C, Clean, "lv_tabview_clean");

   function Add_Tab
     (Self : Instance;
      Name : Interfaces.C.Strings.chars_ptr) return Obj_T;
   pragma Import (C, Add_Tab, "lv_tabview_add_tab");

   procedure Add_Tab
     (Self : Instance;
      Name : Interfaces.C.Strings.chars_ptr) with
      Import        => True,
      Convention    => C,
      External_Name => "lv_tabview_add_tab";

   procedure Set_Tab_Act (Self : Instance; Id : Uint16_T; Anim_En : U_Bool);
   pragma Import (C, Set_Tab_Act, "lv_tabview_set_tab_act");

   procedure Set_Tab_Load_Action
     (Self   : Instance;
      Action : Lv_Tabview_Action_T);
   pragma Import (C, Set_Tab_Load_Action, "lv_tabview_set_tab_load_action");

   procedure Set_Sliding (Self : Instance; En : U_Bool);
   pragma Import (C, Set_Sliding, "lv_tabview_set_sliding");

   procedure Set_Anim_Time (Self : Instance; Anim_Time : Uint16_T);
   pragma Import (C, Set_Anim_Time, "lv_tabview_set_anim_time");

   procedure Set_Style
     (Self  : Instance;
      Typ   : Lv_Tabview_Style_T;
      Style : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_tabview_set_style");

   procedure Set_Btns_Pos (Self : Instance; Btns_Pos : Lv_Tabview_Btns_Pos_T);
   pragma Import (C, Set_Btns_Pos, "lv_tabview_set_btns_pos");

   function Get_Tab_Act (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Tab_Act, "lv_tabview_get_tab_act");

   function Get_Tab_Count (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Tab_Count, "lv_tabview_get_tab_count");

   function Get_Tab (Self : Instance; Id : Uint16_T) return Obj_T;
   pragma Import (C, Get_Tab, "lv_tabview_get_tab");

   function Get_Tab_Load_Action (Self : Instance) return Lv_Tabview_Action_T;
   pragma Import (C, Get_Tab_Load_Action, "lv_tabview_get_tab_load_action");

   function Get_Sliding (Self : Instance) return U_Bool;
   pragma Import (C, Get_Sliding, "lv_tabview_get_sliding");

   function Get_Anim_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Anim_Time, "lv_tabview_get_anim_time");

   function Get_Style
     (Self       : Instance;
      Style_Type : Lv_Tabview_Style_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_tabview_get_style");

   function Get_Btns_Pos (Self : Instance) return Lv_Tabview_Btns_Pos_T;
   pragma Import (C, Get_Btns_Pos, "lv_tabview_get_btns_pos");

end Lv.Objx.Tabview;
