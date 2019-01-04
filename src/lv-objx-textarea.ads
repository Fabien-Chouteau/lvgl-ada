pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lv.Style;
with Interfaces.C.Extensions;

with Interfaces.C.Strings;

with Lv.Objx.Page;
with Lv.Objx.Label;
with Lv.Area;

package Lv.Objx.Textarea is

   subtype Textarea is Obj_T;

   Lv_Ta_Cursor_Last : constant := (16#7FFF#);

   type Cursor_Type_T is
     (Cursor_None,
      Cursor_Line,
      Cursor_Block,
      Cursor_Outline,
      Cursor_Underline,
      Cursor_Hidden) with
        Size => 8;

   for Cursor_Type_T use
     (Cursor_None      => 0,
      Cursor_Line      => 1,
      Cursor_Block     => 2,
      Cursor_Outline   => 3,
      Cursor_Underline => 4,
      Cursor_Hidden    => 8);

   type Ta_Style_T is (Style_Bg, Style_Sb, Style_Cursor) with
        Size => 8;

   function Create (Par : Obj_T; Copy : Obj_T) return Textarea;
   pragma Import (C, Create, "lv_ta_create");

   procedure Add_Char (Self : Textarea; Arg2 : Uint32_T);
   pragma Import (C, Add_Char, "lv_ta_add_char");

   procedure Add_Text (Self : Textarea; Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Add_Text, "lv_ta_add_text");

   procedure Del_Char (Self : Textarea);
   pragma Import (C, Del_Char, "lv_ta_del_char");

   procedure Set_Text (Self : Textarea; Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Text, "lv_ta_set_text");

   procedure Set_Cursor_Pos (Self : Textarea; Arg2 : Int16_T);
   pragma Import (C, Set_Cursor_Pos, "lv_ta_set_cursor_pos");

   procedure Set_Cursor_Type (Self : Textarea; Arg2 : Cursor_Type_T);
   pragma Import (C, Set_Cursor_Type, "lv_ta_set_cursor_type");

   procedure Set_Pwd_Mode (Self : Textarea; Arg2 : U_Bool);
   pragma Import (C, Set_Pwd_Mode, "lv_ta_set_pwd_mode");

   procedure Set_One_Line (Self : Textarea; Arg2 : U_Bool);
   pragma Import (C, Set_One_Line, "lv_ta_set_one_line");

   procedure Set_Text_Align (Self : Textarea; Arg2 : Label.Lv_Label_Align_T);
   pragma Import (C, Set_Text_Align, "lv_ta_set_text_align");

   procedure Set_Accepted_Chars
     (Self : Textarea;
      Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Accepted_Chars, "lv_ta_set_accepted_chars");

   procedure Set_Max_Length (Self : Textarea; Arg2 : Uint16_T);
   pragma Import (C, Set_Max_Length, "lv_ta_set_max_length");

   procedure Set_Action (Self : Textarea; Action : Lv_Action_T);
   pragma Import (C, Set_Action, "lv_ta_set_action_inline");

   procedure Set_Sb_Mode (Self : Textarea; Mode : Page.Lv_Sb_Mode_T);
   pragma Import (C, Set_Sb_Mode, "lv_ta_set_sb_mode_inline");

   procedure Set_Style
     (Self : Textarea;
      Arg2 : Ta_Style_T;
      Arg3 : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_ta_set_style");

   function Get_Text (Self : Textarea) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Text, "lv_ta_get_text");

   function Get_Label (Self : Textarea) return Label.Instance;
   pragma Import (C, Get_Label, "lv_ta_get_label");

   function Get_Cursor_Pos (Self : Textarea) return Uint16_T;
   pragma Import (C, Get_Cursor_Pos, "lv_ta_get_cursor_pos");

   function Get_Cursor_Show (Self : Textarea) return U_Bool;
   pragma Import (C, Get_Cursor_Show, "lv_ta_get_cursor_show");

   function Get_Cursor_Type (Self : Textarea) return Cursor_Type_T;
   pragma Import (C, Get_Cursor_Type, "lv_ta_get_cursor_type");

   function Get_Pwd_Mode (Self : Textarea) return U_Bool;
   pragma Import (C, Get_Pwd_Mode, "lv_ta_get_pwd_mode");

   function Get_One_Line (Self : Textarea) return U_Bool;
   pragma Import (C, Get_One_Line, "lv_ta_get_one_line");

   function Get_Accepted_Chars
     (Self : Textarea) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Accepted_Chars, "lv_ta_get_accepted_chars");

   function Get_Max_Length (Self : Textarea) return Uint16_T;
   pragma Import (C, Get_Max_Length, "lv_ta_get_max_length");

   function Get_Action (Self : Textarea) return Lv_Action_T;
   pragma Import (C, Get_Action, "lv_ta_get_action_inline");

   function Get_Sb_Mode (Self : Textarea) return Page.Lv_Sb_Mode_T;
   pragma Import (C, Get_Sb_Mode, "lv_ta_get_sb_mode_inline");

   function Get_Style
     (Self : Textarea;
      Arg2 : Ta_Style_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_ta_get_style");

   procedure Cursor_Right (Self : Textarea);
   pragma Import (C, Cursor_Right, "lv_ta_cursor_right");

   procedure Cursor_Left (Self : Textarea);
   pragma Import (C, Cursor_Left, "lv_ta_cursor_left");

   procedure Cursor_Down (Self : Textarea);
   pragma Import (C, Cursor_Down, "lv_ta_cursor_down");

   procedure Cursor_Up (Self : Textarea);
   pragma Import (C, Cursor_Up, "lv_ta_cursor_up");

end Lv.Objx.Textarea;
