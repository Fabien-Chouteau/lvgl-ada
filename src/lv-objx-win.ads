with Lv.Style;
with Lv.Area;
with Lv.Objx.Page;
with Lv.Objx.Btn;
with Lv.Objx.Cont;
with System;

package Lv.Objx.Win is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg,
                    Style_Content_Bg,
                    Style_Content_Scrl,
                    Style_Sb,
                    Style_Header,
                    Style_Btn_Rel,
                    Style_Btn_Pr);

   --  Create a window objects
   --  @param par pointer to an object, it will be the parent of the new window
   --  @param copy pointer to a window object, if not NULL then the new object will be copied from it
   --  @return pointer to the created window
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   --  Delete all children of the scrl object, without deleting scrl child.
   --  @param self pointer to an object
   procedure Clean (Self : Instance);

   --  Add control button to the header of the window
   --  @param self pointer to a window object
   --  @param img_src an image source ('lv_img_t' variable, path to file or a symbol)
   --  @param rel_action a function pointer to call when the button is released
   --  @return pointer to the created button object
  function Add_Btn
     (Self       : Instance;
      Img_Src    : System.Address;
      Rel_Action : Action_Func_T) return Btn.Instance;

   --  A release action which can be assigned to a window control button to close it
   --  @param btn pointer to the released button
   --  @return always LV_ACTION_RES_INV because the button is deleted with the window
   function Close_Action (Self : Instance) return Res_T;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the title of a window
   --  @param self pointer to a window object
   --  @param title string of the new title
   procedure Set_Title
     (Self  : Instance;
      Title : C_String_Ptr);

   --  Set the control button size of a window
   --  @param self pointer to a window object
   --  @return control button size
   procedure Set_Btn_Size (Self : Instance; Size : Lv.Area.Coord_T);

   --  Set the layout of the window
   --  @param self pointer to a window object
   --  @param layout the layout from 'lv_layout_t'
   procedure Set_Layout (Self : Instance; Layout : Lv.Objx.Cont.Layout_T);

   --  Set the scroll bar mode of a window
   --  @param self pointer to a window object
   --  @param sb_mode the new scroll bar mode from  'lv_sb_mode_t'
   procedure Set_Sb_Mode (Self : Instance; Sb_Mode : Lv.Objx.Page.Mode_T);

   --  Set a style of a window
   --  @param self pointer to a window object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the title of a window
   --  @param self pointer to a window object
   --  @return title string of the window
   function Title (Self : Instance) return C_String_Ptr;

   --  Get the content holder object of window (`lv_page`) to allow additional customization
   --  @param self pointer to a window object
   --  @return the Page object where the window's content is
   function Content (Self : Instance) return Page.Instance;

   --  Get the control button size of a window
   --  @param self pointer to a window object
   --  @return control button size
   function Btn_Size (Self : Instance) return Lv.Area.Coord_T;

   --  Get the pointer of a widow from one of  its control button.
   --  It is useful in the action of the control buttons where only button is known.
   --  @param ctrl_btn pointer to a control button of a window
   --  @return pointer to the window of 'ctrl_btn'
   function From_Btn (Ctrl_Btn : Btn.Instance) return Instance;

   --  Get the layout of a window
   --  @param self pointer to a window object
   --  @return the layout of the window (from 'lv_layout_t')
   function Layout (Self : Instance) return Lv.Objx.Cont.Layout_T;

   --  Get the scroll bar mode of a window
   --  @param self pointer to a window object
   --  @return the scroll bar mode of the window (from 'lv_sb_mode_t')
   function Sb_Mode (Self : Instance) return Lv.Objx.Page.Mode_T;

   --  Get width of the content area (page scrollable) of the window
   --  @param self pointer to a window object
   --  @return the width of the content area
   function Width (Self : Instance) return Lv.Area.Coord_T;

   --  Get a style of a window
   --  @param self pointer to a button object
   --  @param type which style window be get
   --  @return style pointer to a style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   ---------------------
   -- Other functions --
   ---------------------

   --  Focus on an object. It ensures that the object will be visible in the window.
   --  @param self pointer to a window object
   --  @param obj pointer to an object to focus (must be in the window)
   --  @param anim_time scroll animation time in milliseconds (0: no animation)
   procedure Focus (Self : Instance; Obj : Obj_T; Anim_Time : Uint16_T);

   --  Scroll the window horizontally
   --  @param self pointer to a window object
   --  @param dist the distance to scroll (< 0: scroll right; > 0 scroll left)
   procedure Scroll_Hor (Self : Instance; Dist : Lv.Area.Coord_T);

   --  Scroll the window vertically
   --  @param self pointer to a window object
   --  @param dist the distance to scroll (< 0: scroll down; > 0 scroll up)
   procedure Scroll_Ver (Self : Instance; Dist : Lv.Area.Coord_T);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_win_create");
   pragma Import (C, Clean, "lv_win_clean");
   pragma Import (C, Add_Btn, "lv_win_add_btn");
   pragma Import (C, Close_Action, "lv_win_close_action");
   pragma Import (C, Set_Title, "lv_win_set_title");
   pragma Import (C, Set_Btn_Size, "lv_win_set_btn_size");
   pragma Import (C, Set_Layout, "lv_win_set_layout");
   pragma Import (C, Set_Sb_Mode, "lv_win_set_sb_mode");
   pragma Import (C, Set_Style, "lv_win_set_style");
   pragma Import (C, Title, "lv_win_get_title");
   pragma Import (C, Content, "lv_win_get_content");
   pragma Import (C, Btn_Size, "lv_win_get_btn_size");
   pragma Import (C, From_Btn, "lv_win_get_from_btn");
   pragma Import (C, Layout, "lv_win_get_layout");
   pragma Import (C, Sb_Mode, "lv_win_get_sb_mode");
   pragma Import (C, Width, "lv_win_get_width");
   pragma Import (C, Style, "lv_win_get_style");
   pragma Import (C, Focus, "lv_win_focus");
   pragma Import (C, Scroll_Hor, "lv_win_scroll_hor_inline");
   pragma Import (C, Scroll_Ver, "lv_win_scroll_ver_inline");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg           => 0,
                    Style_Content_Bg   => 1,
                    Style_Content_Scrl => 2,
                    Style_Sb           => 3,
                    Style_Header       => 4,
                    Style_Btn_Rel      => 5,
                    Style_Btn_Pr       => 6);

end Lv.Objx.Win;
