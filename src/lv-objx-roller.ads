with Lv.Style;

package Lv.Objx.Roller is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg, Style_Sel);

   --  Create a roller object
   --  @param par pointer to an object, it will be the parent of the new roller
   --  @param copy pointer to a roller object, if not NULL then the new object will be copied from it
   --  @return pointer to the created roller
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the options on a roller
   --  @param self pointer to roller object
   --  @param options a string with '\n' separated options. E.g. "One\nTwo\nThree"
   procedure Set_Options
     (Self    : Instance;
      Options : C_String_Ptr);

   --  Set the selected option
   --  @param self pointer to a roller object
   --  @param sel_opt id of the selected option (0 ... number of option - 1);
   --  @param anim_en true: set with animation; false set immediately
   procedure Set_Selected (Self : Instance; Set_Opt : Uint16_T; Anim_En : U_Bool);

   --  Set a function to call when a new option is chosen
   --  @param self pointer to a roller
   --  @param action pointer to a callback function
   procedure Set_Action (Self : Instance; Action : Action_Func_T);

   --  Set the height to show the given number of rows (options)
   --  @param self pointer to a roller object
   --  @param row_cnt number of desired visible rows
   procedure Set_Visible_Row_Count (Self : Instance; Arg2 : Uint8_T);

   --  Enable or disable the horizontal fit to the content
   --  @param self pointer to a roller
   --  @param fit en true: enable auto fit; false: disable auto fit
   procedure Set_Hor_Fit (Self : Instance; Fit_En : U_Bool);

   --  Set the open/close animation time.
   --  @param self pointer to a roller object
   --  @param anim_time: open/close animation time [ms]
   procedure Set_Anim_Time (Self : Instance; Anim_Time : Uint16_T);

   --  Set a style of a roller
   --  @param self pointer to a roller object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the options of a roller
   --  @param self pointer to roller object
   --  @return the options separated by '\n'-s (E.g. "Option1\nOption2\nOption3")
   function Options
     (Self : Instance) return C_String_Ptr;

   --  Get the id of the selected option
   --  @param self pointer to a roller object
   --  @return id of the selected option (0 ... number of option - 1);
   function Selected (Self : Instance) return Uint16_T;

   --  Get the current selected option as a string
   --  @param self pointer to roller object
   --  @param buf pointer to an array to store the string
   procedure Selected_Str
     (Self : Instance;
      Buf  : C_String_Ptr);

   --  Get the "option selected" callback function
   --  @param self pointer to a roller
   --  @return  pointer to the call back function
   function Action (Self : Instance) return Action_Func_T;

   --  Get the open/close animation time.
   --  @param self pointer to a roller
   --  @return open/close animation time [ms]
   function Anim_Time (Self : Instance) return Uint16_T;

   --  Get the auto width set attribute
   --  @param self pointer to a roller object
   --  @return true: auto size enabled; false: manual width settings enabled
   function Hor_Fit (Self : Instance) return U_Bool;

   --  Get a style of a roller
   --  @param self pointer to a roller object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_roller_create");
   pragma Import (C, Set_Options, "lv_roller_set_options_inline");
   pragma Import (C, Set_Selected, "lv_roller_set_selected");
   pragma Import (C, Set_Action, "lv_roller_set_action_inline");
   pragma Import (C, Set_Visible_Row_Count, "lv_roller_set_visible_row_count");
   pragma Import (C, Set_Hor_Fit, "lv_roller_set_hor_fit_inline");
   pragma Import (C, Set_Anim_Time, "lv_roller_set_anim_time_inline");
   pragma Import (C, Set_Style, "lv_roller_set_style");
   pragma Import (C, Options, "lv_roller_get_options_inline");
   pragma Import (C, Selected, "lv_roller_get_selected_inline");
   pragma Import (C, Selected_Str, "lv_roller_get_selected_str_inline");
   pragma Import (C, Action, "lv_roller_get_action_inline");
   pragma Import (C, Anim_Time, "lv_roller_get_anim_time_inline");
   pragma Import (C, Hor_Fit, "lv_roller_get_hor_fit");
   pragma Import (C, Style, "lv_roller_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg  => 0,
                    Style_Sel => 1);

end Lv.Objx.Roller;
