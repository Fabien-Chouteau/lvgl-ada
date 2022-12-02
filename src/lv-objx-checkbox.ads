with Lv.Style;

package Lv.Objx.Checkbox is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg,
                    Style_Rel,
                    Style_Pr,
                    Style_Tgl_Rel,
                    Style_Tgl_Pr,
                    Style_Ina);

   --  Create a check box objects
   --  @param par pointer to an object, it will be the parent of the new check box
   --  @param copy pointer to a check box object, if not NULL then the new object will be copied from it
   --  @return pointer to the created check box
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the text of a check box
   --  @param self pointer to a check box
   --  @param txt the text of the check box
   procedure Set_Text (Self : Instance; Txt : C_String_Ptr);

   --  Set the state of the check box
   --  @param self pointer to a check box object
   --  @param checked true: make the check box checked; false: make it unchecked
   procedure Set_Checked (Self : Instance; Checked : U_Bool);

   --  Make the check box inactive (disabled)
   --  @param self pointer to a check box object
   procedure Set_Inactive (Self : Instance);

   --  Set a function to call when the check box is clicked
   --  @param self pointer to a check box object
   procedure Set_Action (Self : Instance; Action : Action_Func_T);

   --  Set a style of a check box
   --  @param self pointer to check box object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the text of a check box
   --  @param self pointer to check box object
   --  @return pointer to the text of the check box
   function Text (Self : Instance) return C_String_Ptr;

   --  Get the current state of the check box
   --  @param self pointer to a check box object
   --  @return true: checked; false: not checked
   function Is_Checked (Self : Instance) return U_Bool;

   --  Get the action of a check box
   --  @param self pointer to a button object
   --  @return pointer to the action function
   function Action (Self : Instance) return Action_Func_T;

   --  Get a style of a button
   --  @param self pointer to check box object
   --  @param type which style should be get
   --  @return style pointer to the style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_cb_create");
   pragma Import (C, Set_Text, "lv_cb_set_text");
   pragma Import (C, Set_Checked, "lv_cb_set_checked_inline");
   pragma Import (C, Set_Inactive, "lv_cb_set_inactive_inline");
   pragma Import (C, Set_Action, "lv_cb_set_action_inline");
   pragma Import (C, Set_Style, "lv_cb_set_style");
   pragma Import (C, Text, "lv_cb_get_text");
   pragma Import (C, Is_Checked, "lv_cb_is_checked_inline");
   pragma Import (C, Action, "lv_cb_get_action_inline");
   pragma Import (C, Style, "lv_cb_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg      => 0,
                    Style_Rel     => 1,
                    Style_Pr      => 2,
                    Style_Tgl_Rel => 3,
                    Style_Tgl_Pr  => 4,
                    Style_Ina     => 5);

end Lv.Objx.Checkbox;
