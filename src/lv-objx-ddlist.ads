with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with Lv.Style;
with Lv.Area;
with Lv.Objx.Page;

package Lv.Objx.Ddlist is

   subtype Instance is Obj_T;

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_ddlist_create");

   procedure Set_Options
     (Self    : Instance;
      Options : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Options, "lv_ddlist_set_options");

   procedure Set_Selected (Self : Instance; Sel_Opt : Uint16_T);
   pragma Import (C, Set_Selected, "lv_ddlist_set_selected");

   procedure Set_Action (Self : Instance; Action : Lv_Action_T);
   pragma Import (C, Set_Action, "lv_ddlist_set_action");

   procedure Set_Fix_Height (Self : Instance; H : Lv.Area.Coord_T);
   pragma Import (C, Set_Fix_Height, "lv_ddlist_set_fix_height");

   procedure Set_Hor_Fit (Self : Instance; Fit_En : U_Bool);
   pragma Import (C, Set_Hor_Fit, "lv_ddlist_set_hor_fit");

   procedure Set_Sb_Mode (Self : Instance; Mode : Lv.Objx.Page.Lv_Sb_Mode_T);
   pragma Import (C, Set_Sb_Mode, "lv_ddlist_set_sb_mode_inline");

   procedure Set_Anim_Time (Self : Instance; Anim_Time : Uint16_T);
   pragma Import (C, Set_Anim_Time, "lv_ddlist_set_anim_time");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_ddlist_set_style");

   function Get_Options
     (Self : Instance) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Options, "lv_ddlist_get_options");

   function Get_Selected (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Selected, "lv_ddlist_get_selected");

   procedure Get_Selected_Str
     (Self : Instance;
      Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Get_Selected_Str, "lv_ddlist_get_selected_str");

   function Get_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Action, "lv_ddlist_get_action");

   function Get_Fix_Height (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Fix_Height, "lv_ddlist_get_fix_height");

   function Get_Sb_Mode (Self : Instance) return Lv.Objx.Page.Lv_Sb_Mode_T;
   pragma Import (C, Get_Sb_Mode, "lv_ddlist_get_sb_mode_inline");

   function Get_Anim_Time (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Anim_Time, "lv_ddlist_get_anim_time");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_ddlist_get_style");

   procedure Open (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Open, "lv_ddlist_open");

   procedure Close (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Close, "lv_ddlist_close");

end Lv.Objx.Ddlist;
