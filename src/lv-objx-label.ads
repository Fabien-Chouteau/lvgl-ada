with Lv.Area;
with Lv.Style;

package Lv.Objx.Label is

   subtype Instance is Obj_T;

   Lv_Label_Dot_Num  : constant := 3;
   Lv_Label_Pos_Last : constant := 16#FFFF#;

   type Long_Mode_T is (Long_Expand, -- Expand the object size to the text size
                        Long_Break,  -- Keep the object width, break the too long lines and expand the object height
                        Long_Scroll, -- Expand the object size and scroll the text on the parent (move the label object)
                        Long_Dot,    -- Keep the size and write dots at the end if the text is too long
                        Long_Roll,   -- Keep the size and roll the text infinitely
                        Long_Crop);  -- Keep the size and crop the text out of it

   type Align_T is (Align_Left, Align_Center, Align_Right);

   --  Create a label objects
   --  @param par pointer to an object, it will be the parent of the new label
   --  @param copy pointer to a button object, if not NULL then the new object will be copied from it
   --  @return pointer to the created button
   function Create (Par : Obj_T; Copy : Obj_T) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set a new text for a label. Memory will be allocated to store the text by the label.
   --  @param self pointer to a label object
   --  @param text '\0' terminated character string. NULL to refresh with the current text.
   procedure Set_Text (Self : Instance; Text : C_String_Ptr);

   --  Set a new text for a label from a character array. The array don't has to be '\0' terminated.
   --  Memory will be allocated to store the array by the label.
   --  @param self pointer to a label object
   --  @param array array of characters or NULL to refresh the label
   --  @param size the size of 'array' in bytes
  procedure Set_Array_Text
     (Self : Instance;
      Arr  : C_String_Ptr;
      Size : Uint16_T);

   --  Set a static text. It will not be saved by the label so the 'text' variable
   --  has to be 'alive' while the label exist.
   --  @param self pointer to a label object
   --  @param text pointer to a text. NULL to refresh with the current text.
  procedure Set_Static_Text
     (Self : Instance;
      Text : C_String_Ptr);

   --  Set the behavior of the label with longer text then the object size
   --  @param self pointer to a label object
   --  @param long_mode the new mode from 'lv_label_long_mode' enum.
   --                   In LV_LONG_BREAK/LONG/ROLL the size of the label should be set AFTER this function
   procedure Set_Long_Mode (Self : Instance; Long_Mode : Long_Mode_T);

   --  Set the align of the label (left or center)
   --  @param self pointer to a label object
   --  @param align 'LV_LABEL_ALIGN_LEFT' or 'LV_LABEL_ALIGN_LEFT'
   procedure Set_Align (Self : Instance; Align : Align_T);

   --  Enable the recoloring by in-line commands
   --  @param self pointer to a label object
   --  @param recolor_en true: enable recoloring, false: disable
   procedure Set_Recolor (Self : Instance; Recolor_En : U_Bool);

   --  Set the label to draw (or not draw) background specified in its style's body
   --  @param self pointer to a label object
   --  @param body_en true: draw body; false: don't draw body
   procedure Set_Body_Draw (Self : Instance; Body_En : U_Bool);

   --  Set the label's animation speed in LV_LABEL_LONG_ROLL and SCROLL modes
   --  @param self pointer to a label object
   --  @param anim_speed speed of animation in px/sec unit
   procedure Set_Anim_Speed (Self : Instance; Anim_Speed : Uint16_T);

   --  Set the style of an label
   --  @param self pointer to an label object
   --  @param style pointer to a style
   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the text of a label
   --  @param self pointer to a label object
   --  @return the text of the label
   function Text (Self : Instance) return C_String_Ptr;

   --  Get the long mode of a label
   --  @param self pointer to a label object
   --  @return the long mode
   function Long_Mode (Self : Instance) return Long_Mode_T;

   --  Get the align attribute
   --  @param self pointer to a label object
   --  @return LV_LABEL_ALIGN_LEFT or LV_LABEL_ALIGN_CENTER
   function Align (Self : Instance) return Align_T;

   --  Get the recoloring attribute
   --  @param self pointer to a label object
   --  @return true: recoloring is enabled, false: disable
   function Recolor (Self : Instance) return U_Bool;

   --  Get the body draw attribute
   --  @param self pointer to a label object
   --  @return true: draw body; false: don't draw body
   function Body_Draw (Self : Instance) return U_Bool;

   --  Get the label's animation speed in LV_LABEL_LONG_ROLL and SCROLL modes
   --  @param self pointer to a label object
   --  @return speed of animation in px/sec unit
   function Anim_Speed (Self : Instance) return Uint16_T;

   --  Get the relative x and y coordinates of a letter
   --  @param self pointer to a label object
   --  @param index index of the letter [0 ... text length]. Expressed in character index, not byte index (different in UTF-8)
   --  @param pos store the result here (E.g. index = 0 gives 0;0 coordinates)
   procedure Letter_Pos
     (Self  : Instance;
      Index : Uint16_T;
      Pos   : access Lv.Area.Point_T);

   --  Get the index of letter on a relative point of a label
   --  @param self pointer to label object
   --  @param pos pointer to point with coordinates on a the label
   --  @return the index of the letter on the 'pos_p' point (E.g. on 0;0 is the 0. letter)
   --  Expressed in character index and not byte index (different in UTF-8)
   function Letter_On
     (Self : Instance;
      Pos  : access Lv.Area.Point_T) return Uint16_T;

   --  Get the style of an label object
   --  @param self pointer to an label object
   --  @return pointer to the label's style
   function Style (Self : Instance) return Lv.Style.Style;

   ---------------------
   -- Other functions --
   ---------------------

   --  Insert a text to the label. The label text can not be static.
   --  @param self pointer to a label object
   --  @param pos character index to insert. Expressed in character index and not byte index (Different in UTF-8)
   --             0: before first char.
   --             LV_LABEL_POS_LAST: after last char.
   --  @param txt pointer to the text to insert
   procedure Ins_Text
     (Self : Instance;
      Pos  : Uint32_T;
      Txt  : C_String_Ptr);

   --  Delete characters from a label. The label text can not be static.
   --  @param self pointer to a label object
   --  @param pos character index to insert. Expressed in character index and not byte index (Different in UTF-8)
   --             0: before first char.
   --  @param cnt number of characters to cut
   procedure Cut_Text (Self : Instance; Pos : Uint32_T; Cnt : Uint32_T);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_label_create");
   pragma Import (C, Set_Text, "lv_label_set_text");
   pragma Import (C, Set_Array_Text, "lv_label_set_array_text");
   pragma Import (C, Set_Static_Text, "lv_label_set_static_text");
   pragma Import (C, Set_Long_Mode, "lv_label_set_long_mode");
   pragma Import (C, Set_Align, "lv_label_set_align");
   pragma Import (C, Set_Recolor, "lv_label_set_recolor");
   pragma Import (C, Set_Body_Draw, "lv_label_set_body_draw");
   pragma Import (C, Set_Anim_Speed, "lv_label_set_anim_speed");
   pragma Import (C, Set_Style, "lv_label_set_style_inline");
   pragma Import (C, Text, "lv_label_get_text");
   pragma Import (C, Long_Mode, "lv_label_get_long_mode");
   pragma Import (C, Align, "lv_label_get_align");
   pragma Import (C, Recolor, "lv_label_get_recolor");
   pragma Import (C, Body_Draw, "lv_label_get_body_draw");
   pragma Import (C, Anim_Speed, "lv_label_get_anim_speed");
   pragma Import (C, Letter_Pos, "lv_label_get_letter_pos");
   pragma Import (C, Letter_On, "lv_label_get_letter_on");
   pragma Import (C, Style, "lv_label_get_style_inline");
   pragma Import (C, Ins_Text, "lv_label_ins_text");
   pragma Import (C, Cut_Text, "lv_label_cut_text");

   for Long_Mode_T'Size use 8;
   for Long_Mode_T use (Long_Expand => 0,
                        Long_Break  => 1,
                        Long_Scroll => 2,
                        Long_Dot    => 3,
                        Long_Roll   => 4,
                        Long_Crop   => 5);

   for Align_T'Size use 8;
   for Align_T use (Align_Left   => 0,
                    Align_Center => 1,
                    Align_Right  => 2);

end Lv.Objx.Label;
