with Lv.Style;

with Lv.Objx.Page;
with Lv.Objx.Label;

package Lv.Objx.Textarea is

   subtype Instance is Obj_T;

   Cursor_Last : constant := 16#7FFF#;

   type Cursor_Type_T is (Cursor_None,
                          Cursor_Line,
                          Cursor_Block,
                          Cursor_Outline,
                          Cursor_Underline,
                          Cursor_Hidden);

   type Style_T is (Style_Bg,
                    Style_Sb,
                    Style_Cursor);

   --  Create a text area objects
   --  @param par pointer to an object, it will be the parent of the new text area
   --  @param copy pointer to a text area object, if not NULL then the new object will be copied from it
   --  @return pointer to the created text area
   function Create (Par : Obj_T; Copy : Instance) return Instance;

   --  Insert a character to the current cursor position.
   --  To add a wide char, e.g. 'Á' use `lv_txt_encoded_conv_wc('Á')`
   --  @param self pointer to a text area object
   --  @param c a character (e.g. 'a')
   procedure Add_Char (Self : Instance; C : Uint32_T);

   --  Insert a text to the current cursor position
   --  @param self pointer to a text area object
   --  @param txt a '\0' terminated string to insert
   procedure Add_Text (Self : Instance; Txt : C_String_Ptr);

   --  Delete a the left character from the current cursor position
   --  @param self pointer to a text area object
   procedure Del_Char (Self : Instance);

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the text of a text area
   --  @param self pointer to a text area
   --  @param txt pointer to the text
   procedure Set_Text (Self : Instance; Txt : C_String_Ptr);

   --  Set the cursor position
   --  @param obj pointer to a text area object
   --  @param pos the new cursor position in character index
   --              < 0 : index from the end of the text
   --              LV_TA_CURSOR_LAST: go after the last character
   procedure Set_Cursor_Pos (Self : Instance; Pos : Int16_T);

   --  Set the cursor type.
   --  @param self pointer to a text area object
   --  @param cur_type: element of 'lv_cursor_type_t'
   procedure Set_Cursor_Type (Self : Instance; Cur_Type : Cursor_Type_T);

   --  Enable/Disable password mode
   --  @param self pointer to a text area object
   --  @param pwd_en true: enable, false: disable
   procedure Set_Pwd_Mode (Self : Instance; Pwd_En : U_Bool);

   --  Configure the text area to one line or back to normal
   --  @param self pointer to a Text area object
   --  @param en true: one line, false: normal
   procedure Set_One_Line (Self : Instance; En : U_Bool);

   --  Set the alignment of the text area.
   --  In one line mode the text can be scrolled only with `LV_LABEL_ALIGN_LEFT`.
   --  This function should be called if the size of text area changes.
   --  @param self pointer to a text are object
   --  @param align the desired alignment from `lv_label_align_t`. (LV_LABEL_ALIGN_LEFT/CENTER/RIGHT)
   procedure Set_Text_Align (Self : Instance; Align : Label.Align_T);

   --  Set a list of characters. Only these characters will be accepted by the text area
   --  @param self pointer to  Text Area
   --  @param list list of characters. Only the pointer is saved. E.g. "+-.,0123456789"
   procedure Set_Accepted_Chars
     (Self : Instance;
      List : C_String_Ptr);

   --  Set max length of a Text Area.
   --  @param self pointer to  Text Area
   --  @param num the maximal number of characters can be added (`lv_ta_set_text` ignores it)
   procedure Set_Max_Length (Self : Instance; Num : Uint16_T);

   --  Set an action to call when the Text area is clicked
   --  @param self pointer to a Text area
   --  @param action a function pointer
   procedure Set_Action (Self : Instance; Action : Action_Func_T);

   --  Set the scroll bar mode of a text area
   --  @param self pointer to a text area object
   --  @param sb_mode the new mode from 'lv_page_sb_mode_t' enum
   procedure Set_Sb_Mode (Self : Instance; Mode : Page.Mode_T);

   --  Set a style of a text area
   --  @param self pointer to a text area object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the text of a text area. In password mode it gives the real text (not '*'s).
   --  @param self pointer to a text area object
   --  @return pointer to the text
   function Text (Self : Instance) return C_String_Ptr;

   --  Get the label of a text area
   --  @param self pointer to a text area object
   --  @return pointer to the label object
   function Label (Self : Instance) return Label.Instance;

   --  Get the current cursor position in character index
   --  @param self pointer to a text area object
   --  @return the cursor position
   function Cursor_Pos (Self : Instance) return Uint16_T;

   --  Get the current cursor visibility.
   --  @param self pointer to a text area object
   --  @return true: the cursor is drawn, false: the cursor is hidden
   function Cursor_Show (Self : Instance) return U_Bool;

   --  Get the current cursor type.
   --  @param self pointer to a text area object
   --  @return element of 'lv_cursor_type_t'
   function Cursor_Type (Self : Instance) return Cursor_Type_T;

   --  Get the password mode attribute
   --  @param self pointer to a text area object
   --  @return true: password mode is enabled, false: disabled
   function Pwd_Mode (Self : Instance) return U_Bool;

   --  Get the one line configuration attribute
   --  @param self pointer to a text area object
   --  @return true: one line configuration is enabled, false: disabled
   function One_Line (Self : Instance) return U_Bool;

   --  Get a list of accepted characters.
   --  @param self pointer to  Text Area
   --  @return list of accented characters.
   function Accepted_Chars
     (Self : Instance) return C_String_Ptr;

   --  Set max length of a Text Area.
   --  @param self pointer to  Text Area
   --  @return the maximal number of characters to be add
   function Max_Length (Self : Instance) return Uint16_T;

   --  Set an action to call when the Text area is clicked
   --  @param self pointer to a Text area
   --  @param action a function pointer
   function Action (Self : Instance) return Action_Func_T;

   --  Get the scroll bar mode of a text area
   --  @param self pointer to a text area object
   --  @return scrollbar mode from 'lv_page_sb_mode_t' enum
   function Sb_Mode (Self : Instance) return Page.Mode_T;

   --  Get a style of a text area
   --  @param self pointer to a text area object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   --  Move the cursor one character right
   --  @param self pointer to a text area object
   procedure Cursor_Right (Self : Instance);

   --  Move the cursor one character left
   --  @param self pointer to a text area object
   procedure Cursor_Left (Self : Instance);

   --  Move the cursor one line down
   --  @param self pointer to a text area object
   procedure Cursor_Down (Self : Instance);

   --  Move the cursor one line up
   --  @param self pointer to a text area object
   procedure Cursor_Up (Self : Instance);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_ta_create");
   pragma Import (C, Add_Char, "lv_ta_add_char");
   pragma Import (C, Add_Text, "lv_ta_add_text");
   pragma Import (C, Del_Char, "lv_ta_del_char");
   pragma Import (C, Set_Text, "lv_ta_set_text");
   pragma Import (C, Set_Cursor_Pos, "lv_ta_set_cursor_pos");
   pragma Import (C, Set_Cursor_Type, "lv_ta_set_cursor_type");
   pragma Import (C, Set_Pwd_Mode, "lv_ta_set_pwd_mode");
   pragma Import (C, Set_One_Line, "lv_ta_set_one_line");
   pragma Import (C, Set_Text_Align, "lv_ta_set_text_align");
   pragma Import (C, Set_Accepted_Chars, "lv_ta_set_accepted_chars");
   pragma Import (C, Set_Max_Length, "lv_ta_set_max_length");
   pragma Import (C, Set_Action, "lv_ta_set_action_inline");
   pragma Import (C, Set_Sb_Mode, "lv_ta_set_sb_mode_inline");
   pragma Import (C, Set_Style, "lv_ta_set_style");
   pragma Import (C, Text, "lv_ta_get_text");
   pragma Import (C, Label, "lv_ta_get_label");
   pragma Import (C, Cursor_Pos, "lv_ta_get_cursor_pos");
   pragma Import (C, Cursor_Show, "lv_ta_get_cursor_show");
   pragma Import (C, Cursor_Type, "lv_ta_get_cursor_type");
   pragma Import (C, Pwd_Mode, "lv_ta_get_pwd_mode");
   pragma Import (C, One_Line, "lv_ta_get_one_line");
   pragma Import (C, Accepted_Chars, "lv_ta_get_accepted_chars");
   pragma Import (C, Max_Length, "lv_ta_get_max_length");
   pragma Import (C, Action, "lv_ta_get_action_inline");
   pragma Import (C, Sb_Mode, "lv_ta_get_sb_mode_inline");
   pragma Import (C, Style, "lv_ta_get_style");
   pragma Import (C, Cursor_Right, "lv_ta_cursor_right");
   pragma Import (C, Cursor_Left, "lv_ta_cursor_left");
   pragma Import (C, Cursor_Down, "lv_ta_cursor_down");
   pragma Import (C, Cursor_Up, "lv_ta_cursor_up");

   for Cursor_Type_T'Size use 8;
   for Cursor_Type_T use
     (Cursor_None      => 0,
      Cursor_Line      => 1,
      Cursor_Block     => 2,
      Cursor_Outline   => 3,
      Cursor_Underline => 4,
      Cursor_Hidden    => 8);

   for Style_T'Size use 8;
   for Style_T use (Style_Bg     => 0,
                       Style_Sb     => 1,
                       Style_Cursor => 2);

end Lv.Objx.Textarea;
