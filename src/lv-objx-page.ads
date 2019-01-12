with Lv.Style;
with Lv.Area;

with Lv.Objx.Cont;

package Lv.Objx.Page is

   subtype Instance is Obj_T;

   --  Scrollbar modes: shows when should the scrollbars be visible
   type Mode_T is
     (Sb_Mode_Off,     --  Never show scrollbars
      Sb_Mode_On,      --  Always show scrollbars
      Sb_Mode_Drag,    --  Show scrollbars when page is being dragged
      Sb_Mode_Auto,    --  Show scrollbars when the scrollable container is large enough to be scrolled
      Sb_Mode_Hide,    --  Hide the scroll bar temporally
      Sb_Mode_Unhide); --  Unhide the previously hidden scrollbar. Recover it's type too

   type Style_T is (Style_Bg,
                    Style_Scrl,
                    Style_Sb);

   --  Create a page objects
   --  @param par pointer to an object, it will be the parent of the new page
   --  @param copy pointer to a page object, if not NULL then the new object will be copied from it
   --  @return pointer to the created page
   function Create (Par : Obj_T; Copy : Instance) return Instance;

   --  Delete all children of the scrl object, without deleting scrl child.
   --  @param self pointer to an object
   procedure Clean (Self : Instance);

   --  Get the press action of the page
   --  @param self pointer to a page object
   --  @return a function to call when the page is pressed
   function Pr_Action (Self : Instance) return Action_Func_T;

   --  Get the release action of the page
   --  @param self pointer to a page object
   --  @return a function to call when the page is released
   function Rel_Action (Self : Instance) return Action_Func_T;

   --  Get the scrollable object of a page
   --  @param self pointer to a page object
   --  @return pointer to a container which is the scrollable part of the page
   function Scrl (Arg1 : Instance) return Obj_T;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set a release action for the page
   --  @param self pointer to a page object
   --  @param rel_action a function to call when the page is released
   procedure Set_Rel_Action (Self : Instance; Rel_Action : Action_Func_T);

   --  Set a press action for the page
   --  @param self pointer to a page object
   --  @param pr_action a function to call when the page is pressed
   procedure Set_Pr_Action (Self : Instance; Pr_Action : Action_Func_T);

   --  Set the scroll bar mode on a page
   --  @param self pointer to a page object
   --  @param sb_mode the new mode from 'lv_page_sb.mode_t' enum
   procedure Set_Sb_Mode (Self : Instance; Sb_Mode : Mode_T);

   --  Enable/Disable scrolling with arrows if the page is in group (arrows: LV_GROUP_KEY_LEFT/RIGHT/UP/DOWN)
   --  @param self pointer to a page object
   --  @param en true: enable scrolling with arrows
   procedure Set_Arrow_Scroll (Self : Instance; En : U_Bool);

   --  Set the fit attribute of the scrollable part of a page.
   --  It means it can set its size automatically to involve all children.
   --  (Can be set separately horizontally and vertically)
   --  @param self pointer to a page object
   --  @param hor_en true: enable horizontal fit
   --  @param ver_en true: enable vertical fit
   procedure Set_Scrl_Fit (Self : Instance; Hor_En : U_Bool; Ver_En : U_Bool);

   --  Set width of the scrollable part of a page
   --  @param self pointer to a page object
   --  @param w the new width of the scrollable (it ha no effect is horizontal fit is enabled)
   procedure Set_Scrl_Width (Self : Instance; W : Lv.Area.Coord_T);

   --  Set height of the scrollable part of a page
   --  @param self pointer to a page object
   --  @param h the new height of the scrollable (it ha no effect is vertical fit is enabled)
   procedure Set_Scrl_Height (Self : Instance; H : Lv.Area.Coord_T);

   --  Set the layout of the scrollable part of the page
   --  @param self pointer to a page object
   --  @param layout a layout from 'lv_cont_layout_t'
   procedure Set_Scrl_Layout (Self : Instance; Layout : Lv.Objx.Cont.Layout_T);

   --  Set a style of a page
   --  @param self pointer to a page object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self  : Instance;
      Typ   : Style_T;
      Style : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Set the scroll bar mode on a page
   --  @param self pointer to a page object
   --  @return the mode from 'lv_page_sb.mode_t' enum
   function Sb_Mode (Self : Instance) return Mode_T;

   --  Get the the scrolling with arrows (LV_GROUP_KEY_LEFT/RIGHT/UP/DOWN) is enabled or not
   --  @param self pointer to a page object
   --  @return true: scrolling with arrows is enabled
   function Arrow_Scroll (Self : Instance) return U_Bool;

   --  Get that width which can be set to the children to still not cause overflow (show scrollbars)
   --  @param self pointer to a page object
   --  @return the width which still fits into the page
   function Fit_Width (Self : Instance) return Lv.Area.Coord_T;

   --  Get that height which can be set to the children to still not cause overflow (show scrollbars)
   --  @param self pointer to a page object
   --  @return the height which still fits into the page
   function Fit_Height (Self : Instance) return Lv.Area.Coord_T;

   --  Get width of the scrollable part of a page
   --  @param self pointer to a page object
   --  @return the width of the scrollable
   function Scrl_Width (Self : Instance) return Lv.Area.Coord_T;

   --  Get height of the scrollable part of a page
   --  @param self pointer to a page object
   --  @return the height of the scrollable
   function Scrl_Height (Self : Instance) return Lv.Area.Coord_T;

   --  Get the layout of the scrollable part of a page
   --  @param self pointer to page object
   --  @return the layout from 'lv_cont_layout_t'
   function Scrl_Layout (Self : Instance) return Lv.Objx.Cont.Layout_T;

   --  Get horizontal fit attribute of the scrollable part of a page
   --  @param self pointer to a page object
   --  @return true: horizontal fit is enabled; false: disabled
   function Scrl_Hor_Fit (Self : Instance) return U_Bool;

   --  Get vertical fit attribute of the scrollable part of a page
   --  @param self pointer to a page object
   --  @return true: vertical fit is enabled; false: disabled
   function Scrl_Fit_Ver (Self : Instance) return U_Bool;

   --  Get a style of a page
   --  @param self pointer to page object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   ---------------------
   -- Other functions --
   ---------------------

   --  Glue the object to the page. After it the page can be moved (dragged) with this object too.
   --  @param obj pointer to an object on a page
   --  @param glue true: enable glue, false: disable glue
   procedure Glue_Obj (Self : Obj_T; Glue : U_Bool);

   --  Focus on an object. It ensures that the object will be visible on the page.
   --  @param self pointer to a page object
   --  @param obj pointer to an object to focus (must be on the page)
   --  @param anim_time scroll animation time in milliseconds (0: no animation)
   procedure Focus (Self : Instance; Ob : Obj_T; Anim_Time : Uint16_T);

   --  Scroll the page horizontally
   --  @param self pointer to a page object
   --  @param dist the distance to scroll (< 0: scroll left; > 0 scroll right)
   procedure Scroll_Hor (Self : Instance; Dist : Lv.Area.Coord_T);

   --  Scroll the page vertically
   --  @param self pointer to a page object
   --  @param dist the distance to scroll (< 0: scroll down; > 0 scroll up)
   procedure Scroll_Ver (Self : Instance; Dist : Lv.Area.Coord_T);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_page_create");
   pragma Import (C, Clean, "lv_page_clean");
   pragma Import (C, Pr_Action, "lv_page_get_pr_action");
   pragma Import (C, Rel_Action, "lv_page_get_rel_action");
   pragma Import (C, Scrl, "lv_page_get_scrl");
   pragma Import (C, Set_Rel_Action, "lv_page_set_rel_action");
   pragma Import (C, Set_Pr_Action, "lv_page_set_pr_action");
   pragma Import (C, Set_Sb_Mode, "lv_page_set_sb_mode");
   pragma Import (C, Set_Arrow_Scroll, "lv_page_set_arrow_scroll");
   pragma Import (C, Set_Scrl_Fit, "lv_page_set_scrl_fit_inline");
   pragma Import (C, Set_Scrl_Width, "lv_page_set_scrl_width_inline");
   pragma Import (C, Set_Scrl_Height, "lv_page_set_scrl_height_inline");
   pragma Import (C, Set_Scrl_Layout, "lv_page_set_scrl_layout_inline");
   pragma Import (C, Set_Style, "lv_page_set_style");
   pragma Import (C, Sb_Mode, "lv_page_get_sb_mode");
   pragma Import (C, Arrow_Scroll, "lv_page_get_arrow_scroll");
   pragma Import (C, Fit_Width, "lv_page_get_fit_width");
   pragma Import (C, Fit_Height, "lv_page_get_fit_height");
   pragma Import (C, Scrl_Width, "lv_page_get_scrl_width_inline");
   pragma Import (C, Scrl_Height, "lv_page_get_scrl_height_inline");
   pragma Import (C, Scrl_Layout, "lv_page_get_scrl_layout_inline");
   pragma Import (C, Scrl_Hor_Fit, "lv_page_get_scrl_hor_fit_inline");
   pragma Import (C, Scrl_Fit_Ver, "lv_page_get_scrl_fit_ver_inline");
   pragma Import (C, Style, "lv_page_get_style");
   pragma Import (C, Glue_Obj, "lv_page_glue_obj");
   pragma Import (C, Focus, "lv_page_focus");
   pragma Import (C, Scroll_Hor, "lv_page_scroll_hor");
   pragma Import (C, Scroll_Ver, "lv_page_scroll_ver");

   for Mode_T'Size use 8;
   for Mode_T use
     (Sb_Mode_Off    => 0,
      Sb_Mode_On     => 1,
      Sb_Mode_Drag   => 2,
      Sb_Mode_Auto   => 3,
      Sb_Mode_Hide   => 4,
      Sb_Mode_Unhide => 5);

   for Style_T'Size use 8;
   for Style_T use (Style_Bg   => 0,
                    Style_Scrl => 1,
                    Style_Sb   => 2);

end Lv.Objx.Page;
