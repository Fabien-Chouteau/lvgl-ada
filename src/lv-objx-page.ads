pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lv.Style;
with Lv.Area;
with Interfaces.C.Extensions;

with Lv.Objx.Cont;

package Lv.Objx.Page is

   subtype Instance is Obj_T;

   --  Scrollbar modes: shows when should the scrollbars be visible
   type Lv_Sb_Mode_T is
     (Sb_Mode_Off,   --  Never show scrollbars
      Sb_Mode_On,    --  Always show scrollbars
      Sb_Mode_Drag,  --  Show scrollbars when page is being dragged
      Sb_Mode_Auto,  --  Show scrollbars when the scrollable container is large enough to be scrolled
      Sb_Mode_Hide,  --  Hide the scroll bar temporally
      Sb_Mode_Unhide --  Unhide the previously hidden scrollbar. Recover it's type too
      ) with
        Size => 8;

   for Lv_Sb_Mode_T use
     (Sb_Mode_Off    => 0,
      Sb_Mode_On     => 1,
      Sb_Mode_Drag   => 2,
      Sb_Mode_Auto   => 3,
      Sb_Mode_Hide   => 4,
      Sb_Mode_Unhide => 5);

   type Lv_Page_Style_T is (Style_Bg, Style_Scrl, Style_Sb) with
        Size => 8;

   for Lv_Page_Style_T use (Style_Bg => 0, Style_Scrl => 1, Style_Sb => 2);

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_page_create");

   procedure Clean (Self : Instance);
   pragma Import (C, Clean, "lv_page_clean");

   function Get_Pr_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Pr_Action, "lv_page_get_pr_action");

   function Get_Rel_Action (Self : Instance) return Lv_Action_T;
   pragma Import (C, Get_Rel_Action, "lv_page_get_rel_action");

   function Get_Scrl (Arg1 : Instance) return Obj_T;
   pragma Import (C, Get_Scrl, "lv_page_get_scrl");

   procedure Set_Rel_Action (Self : Instance; Rel_Action : Lv_Action_T);
   pragma Import (C, Set_Rel_Action, "lv_page_set_rel_action");

   procedure Set_Pr_Action (Self : Instance; Pr_Action : Lv_Action_T);
   pragma Import (C, Set_Pr_Action, "lv_page_set_pr_action");

   procedure Set_Sb_Mode (Self : Instance; Sb_Mode : Lv_Sb_Mode_T);
   pragma Import (C, Set_Sb_Mode, "lv_page_set_sb_mode");

   procedure Set_Arrow_Scroll (Self : Instance; En : U_Bool);
   pragma Import (C, Set_Arrow_Scroll, "lv_page_set_arrow_scroll");

   procedure Set_Scrl_Fit (Self : Instance; Hor_En : U_Bool; Ver_En : U_Bool);
   pragma Import (C, Set_Scrl_Fit, "lv_page_set_scrl_fit_inline");

   procedure Set_Scrl_Width (Self : Instance; W : Lv.Area.Coord_T);
   pragma Import (C, Set_Scrl_Width, "lv_page_set_scrl_width_inline");

   procedure Set_Scrl_Height (Self : Instance; H : Lv.Area.Coord_T);
   pragma Import (C, Set_Scrl_Height, "lv_page_set_scrl_height_inline");

   procedure Set_Scrl_Layout (Self : Instance; Layout : Lv.Objx.Cont.Layout_T);
   pragma Import (C, Set_Scrl_Layout, "lv_page_set_scrl_layout_inline");

   procedure Set_Style
     (Self  : Instance;
      Typ   : Lv_Page_Style_T;
      Style : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_page_set_style");

   function Get_Sb_Mode (Self : Instance) return Lv_Sb_Mode_T;
   pragma Import (C, Get_Sb_Mode, "lv_page_get_sb_mode");

   function Get_Arrow_Scroll (Self : Instance) return U_Bool;
   pragma Import (C, Get_Arrow_Scroll, "lv_page_get_arrow_scroll");

   function Get_Fit_Width (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Fit_Width, "lv_page_get_fit_width");

   function Get_Fit_Height (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Fit_Height, "lv_page_get_fit_height");

   function Get_Scrl_Width (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Scrl_Width, "lv_page_get_scrl_width_inline");

   function Get_Scrl_Height (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Scrl_Height, "lv_page_get_scrl_height_inline");

   function Get_Scrl_Layout (Self : Instance) return Lv.Objx.Cont.Layout_T;
   pragma Import (C, Get_Scrl_Layout, "lv_page_get_scrl_layout_inline");

   function Get_Scrl_Hor_Fit (Self : Instance) return U_Bool;
   pragma Import (C, Get_Scrl_Hor_Fit, "lv_page_get_scrl_hor_fit_inline");

   function Get_Scrl_Fit_Ver (Self : Instance) return U_Bool;
   pragma Import (C, Get_Scrl_Fit_Ver, "lv_page_get_scrl_fit_ver_inline");

   function Get_Style
     (Self : Instance;
      Arg2 : Lv_Page_Style_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_page_get_style");

   procedure Glue_Obj (Self : Obj_T; Glue : U_Bool);
   pragma Import (C, Glue_Obj, "lv_page_glue_obj");

   procedure Focus (Self : Instance; Ob : Obj_T; Anim_Time : Uint16_T);
   pragma Import (C, Focus, "lv_page_focus");

   procedure Scroll_Hor (Self : Instance; Dist : Lv.Area.Coord_T);
   pragma Import (C, Scroll_Hor, "lv_page_scroll_hor");

   procedure Scroll_Ver (Self : Instance; Dist : Lv.Area.Coord_T);
   pragma Import (C, Scroll_Ver, "lv_page_scroll_ver");

end Lv.Objx.Page;
