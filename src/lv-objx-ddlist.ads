with Lv.Style;
with Lv.Area;
with Lv.Objx.Page;

package Lv.Objx.Ddlist is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg, Style_Sel, Style_Sb);

   --  Create a drop down list objects
   --  @param par pointer to an object, it will be the parent of the new drop down list
   --  @param copy pointer to a drop down list object, if not NULL then the new object will be copied from it
   --  @return pointer to the created drop down list
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the options in a drop down list from a string
   --  @param self pointer to drop down list object
   --  @param options a string with '\n' separated options. E.g. "One\nTwo\nThree"
   procedure Set_Options
     (Self    : Instance;
      Options : C_String_Ptr);

   --  Set the selected option
   --  @param self pointer to drop down list object
   --  @param sel_opt id of the selected option (0 ... number of option - 1);
   procedure Set_Selected (Self : Instance; Sel_Opt : Uint16_T);

   --  Set a function to call when a new option is chosen
   --  @param self pointer to a drop down list
   --  @param action pointer to a call back function
   procedure Set_Action (Self : Instance; Action : Action_Func_T);

   --  Set the fix height for the drop down list
   --  If 0 then the opened ddlist will be auto. sized else the set height will be applied.
   --  @param self pointer to a drop down list
   --  @param h the height when the list is opened (0: auto size)
   procedure Set_Fix_Height (Self : Instance; H : Lv.Area.Coord_T);

   --  Enable or disable the horizontal fit to the content
   --  @param self pointer to a drop down list
   --  @param fit en true: enable auto fit; false: disable auto fit
   procedure Set_Hor_Fit (Self : Instance; Fit_En : U_Bool);

   --  Set the scroll bar mode of a drop down list
   --  @param self pointer to a drop down list object
   --  @param sb_mode the new mode from 'lv_page_sb_mode_t' enum
   procedure Set_Sb_Mode (Self : Instance; Mode : Lv.Objx.Page.Mode_T);

   --  Set the open/close animation time.
   --  @param self pointer to a drop down list
   --  @param anim_time: open/close animation time [ms]
   procedure Set_Anim_Time (Self : Instance; Anim_Time : Uint16_T);

   --  Set a style of a drop down list
   --  @param self pointer to a drop down list object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the options of a drop down list
   --  @param self pointer to drop down list object
   --  @return the options separated by '\n'-s (E.g. "Option1\nOption2\nOption3")
   function Options
     (Self : Instance) return C_String_Ptr;

   --  Get the selected option
   --  @param self pointer to drop down list object
   --  @return id of the selected option (0 ... number of option - 1);
   function Selected (Self : Instance) return Uint16_T;

   --  Get the current selected option as a string
   --  @param self pointer to ddlist object
   --  @param buf pointer to an array to store the string
   procedure Selected_Str
     (Self : Instance;
      Buf  : C_String_Ptr);

   --  Get the "option selected" callback function
   --  @param self pointer to a drop down list
   --  @return  pointer to the call back function
   function Action (Self : Instance) return Action_Func_T;

   --  Get the fix height value.
   --  @param self pointer to a drop down list object
   --  @return the height if the ddlist is opened (0: auto size)
   function Fix_Height (Self : Instance) return Lv.Area.Coord_T;

   --  Get the scroll bar mode of a drop down list
   --  @param self pointer to a  drop down list object
   --  @return scrollbar mode from 'lv_page_sb_mode_t' enum
   function Sb_Mode (Self : Instance) return Lv.Objx.Page.Mode_T;

   --  Get the open/close animation time.
   --  @param self pointer to a drop down list
   --  @return open/close animation time [ms]
   function Anim_Time (Self : Instance) return Uint16_T;

   --  Get a style of a drop down list
   --  @param self pointer to a drop down list object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self  : Instance;
      Style : Style_T) return Lv.Style.Style;

   ---------------------
   -- Other functions --
   ---------------------

   --  Open the drop down list with or without animation
   --  @param self pointer to drop down list object
   --  @param anim_en true: use animation; false: not use animations
   procedure Open (Self : Instance; Anim_En : U_Bool);

   --  Close (Collapse) the drop down list
   --  @param self pointer to drop down list object
   --  @param anim true: use animation; false: not use animations
   procedure Close (Self : Instance; Anim : U_Bool);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_ddlist_create");
   pragma Import (C, Set_Options, "lv_ddlist_set_options");
   pragma Import (C, Set_Selected, "lv_ddlist_set_selected");
   pragma Import (C, Set_Action, "lv_ddlist_set_action");
   pragma Import (C, Set_Fix_Height, "lv_ddlist_set_fix_height");
   pragma Import (C, Set_Hor_Fit, "lv_ddlist_set_hor_fit");
   pragma Import (C, Set_Sb_Mode, "lv_ddlist_set_sb_mode_inline");
   pragma Import (C, Set_Anim_Time, "lv_ddlist_set_anim_time");
   pragma Import (C, Set_Style, "lv_ddlist_set_style");
   pragma Import (C, Options, "lv_ddlist_get_options");
   pragma Import (C, Selected, "lv_ddlist_get_selected");
   pragma Import (C, Selected_Str, "lv_ddlist_get_selected_str");
   pragma Import (C, Action, "lv_ddlist_get_action");
   pragma Import (C, Fix_Height, "lv_ddlist_get_fix_height");
   pragma Import (C, Sb_Mode, "lv_ddlist_get_sb_mode_inline");
   pragma Import (C, Anim_Time, "lv_ddlist_get_anim_time");
   pragma Import (C, Style, "lv_ddlist_get_style");
   pragma Import (C, Open, "lv_ddlist_open");
   pragma Import (C, Close, "lv_ddlist_close");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg  => 0,
                    Style_Sel => 1,
                    Style_Sb  => 2);

end Lv.Objx.Ddlist;
