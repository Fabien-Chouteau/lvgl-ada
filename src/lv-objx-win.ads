with Interfaces.C; use Interfaces.C;
with Lv.Style;
with Lv.Area;
with Lv.Objx.Page;
with Lv.Objx.Btn;
with Lv.Objx.Cont;
with System;
with Interfaces.C.Strings;

package Lv.Objx.Win is

   subtype Instance is Obj_T;

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_win_create");

   procedure Clean (Self : Instance);
   pragma Import (C, Clean, "lv_win_clean");

   function Add_Btn
     (Self : Instance;
      Arg2 : System.Address;
      Arg3 : Lv_Action_T) return Btn.Instance;
   pragma Import (C, Add_Btn, "lv_win_add_btn");

   function Close_Action (Self : Instance) return Lv_Res_T;
   pragma Import (C, Close_Action, "lv_win_close_action");

   procedure Set_Title
     (Self : Instance;
      Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Title, "lv_win_set_title");

   procedure Set_Btn_Size (Self : Instance; Arg2 : Lv.Area.Coord_T);
   pragma Import (C, Set_Btn_Size, "lv_win_set_btn_size");

   procedure Set_Layout (Self : Instance; Arg2 : Lv.Objx.Cont.Lv_Layout_T);
   pragma Import (C, Set_Layout, "lv_win_set_layout");

   procedure Set_Sb_Mode (Self : Instance; Arg2 : Lv.Objx.Page.Lv_Sb_Mode_T);
   pragma Import (C, Set_Sb_Mode, "lv_win_set_sb_mode");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_win_set_style");

   function Get_Title (Self : Instance) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Title, "lv_win_get_title");

   function Get_Content (Self : Instance) return Page.Instance;
   pragma Import (C, Get_Content, "lv_win_get_content");

   function Get_Btn_Size (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Btn_Size, "lv_win_get_btn_size");

   function Get_From_Btn (Ctrl_Btn : Btn.Instance) return Instance;
   pragma Import (C, Get_From_Btn, "lv_win_get_from_btn");

   function Get_Layout (Self : Instance) return Lv.Objx.Cont.Lv_Layout_T;
   pragma Import (C, Get_Layout, "lv_win_get_layout");

   function Get_Sb_Mode (Self : Instance) return Lv.Objx.Page.Lv_Sb_Mode_T;
   pragma Import (C, Get_Sb_Mode, "lv_win_get_sb_mode");

   function Get_Width (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Width, "lv_win_get_width");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_win_get_style");

   procedure Focus (Self : Instance; Obj : Obj_T; Anim_Time : Uint16_T);
   pragma Import (C, Focus, "lv_win_focus");

   procedure Scroll_Hor (Self : Instance; Dist : Lv.Area.Coord_T);
   pragma Import (C, Scroll_Hor, "lv_win_scroll_hor_inline");

   procedure Scroll_Ver (Self : Instance; Dist : Lv.Area.Coord_T);
   pragma Import (C, Scroll_Ver, "lv_win_scroll_ver_inline");

end Lv.Objx.Win;
