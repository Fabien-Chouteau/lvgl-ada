pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Lv.Area;
with Interfaces.C.Extensions;
with Lv.Style;

package Lv.Objx.Label is

   subtype Instance is Obj_T;

   Lv_Label_Dot_Num  : constant := 3;
   Lv_Label_Pos_Last : constant := 16#FFFF#;

   subtype Lv_Label_Long_Mode_T is Uint8_T;

   subtype Lv_Label_Align_T is Uint8_T;

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_label_create");

   procedure Set_Text (Self : Instance; Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Text, "lv_label_set_text");

   procedure Set_Array_Text
     (Self : Instance;
      Arg2 : Interfaces.C.Strings.chars_ptr;
      Arg3 : Uint16_T);
   pragma Import (C, Set_Array_Text, "lv_label_set_array_text");

   procedure Set_Static_Text
     (Self : Instance;
      Arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Set_Static_Text, "lv_label_set_static_text");

   procedure Set_Long_Mode (Self : Instance; Arg2 : Lv_Label_Long_Mode_T);
   pragma Import (C, Set_Long_Mode, "lv_label_set_long_mode");

   procedure Set_Align (Self : Instance; Arg2 : Lv_Label_Align_T);
   pragma Import (C, Set_Align, "lv_label_set_align");

   procedure Set_Recolor (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Set_Recolor, "lv_label_set_recolor");

   procedure Set_Body_Draw (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Set_Body_Draw, "lv_label_set_body_draw");

   procedure Set_Anim_Speed (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Anim_Speed, "lv_label_set_anim_speed");

   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_label_set_style_inline");

   function Get_Text (Self : Instance) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Get_Text, "lv_label_get_text");

   function Get_Long_Mode (Self : Instance) return Lv_Label_Long_Mode_T;
   pragma Import (C, Get_Long_Mode, "lv_label_get_long_mode");

   function Get_Align (Self : Instance) return Lv_Label_Align_T;
   pragma Import (C, Get_Align, "lv_label_get_align");

   function Get_Recolor (Self : Instance) return U_Bool;
   pragma Import (C, Get_Recolor, "lv_label_get_recolor");

   function Get_Body_Draw (Self : Instance) return U_Bool;
   pragma Import (C, Get_Body_Draw, "lv_label_get_body_draw");

   function Get_Anim_Speed (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Anim_Speed, "lv_label_get_anim_speed");

   procedure Get_Letter_Pos
     (Self : Instance;
      Arg2 : Uint16_T;
      Arg3 : access Lv.Area.Point_T);
   pragma Import (C, Get_Letter_Pos, "lv_label_get_letter_pos");

   function Get_Letter_On
     (Self : Instance;
      Arg2 : access Lv.Area.Point_T) return Uint16_T;
   pragma Import (C, Get_Letter_On, "lv_label_get_letter_on");

   function Get_Style (Self : Instance) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_label_get_style_inline");

   procedure Ins_Text
     (Self : Instance;
      Arg2 : Uint32_T;
      Arg3 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Ins_Text, "lv_label_ins_text");

   procedure Cut_Text (Self : Instance; Arg2 : Uint32_T; Arg3 : Uint32_T);
   pragma Import (C, Cut_Text, "lv_label_cut_text");

end Lv.Objx.Label;
