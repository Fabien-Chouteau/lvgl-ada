pragma Style_Checks (Off);

with Lv.Style;
with System;

with Lv.Objx.Label;
with Lv.Objx.Page;
with Lv.Objx.Btn;

package Lv.Objx.List is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg,
                    Style_Scrl,
                    Style_Sb,
                    Style_Btn_Rel,
                    Style_Btn_Pr,
                    Style_Btn_Tgl_Rel,
                    Style_Btn_Tgl_Pr,
                    Style_Btn_Ina);

   --  Create a list objects
   --  @param par pointer to an object, it will be the parent of the new list
   --  @param copy pointer to a list object, if not NULL then the new object will be copied from it
   --  @return pointer to the created list
   function Create (Par : Obj_T; Copy : Instance) return Instance;

   --  Delete all children of the scrl object, without deleting scrl child.
   --  @param obj pointer to an object
   procedure Clean (Self : Instance);

   --  Add a list element to the list
   --  @param self pointer to list object
   --  @param img_fn file name of an image before the text (NULL if unused)
   --  @param txt text of the list element (NULL if unused)
   --  @param rel_action pointer to release action function (like with lv_btn)
   --  @return pointer to the new list element which can be customized (a button)
   function Add
     (Self       : Instance;
      Img_Gn     : System.Address;
      Txt        : C_String_Ptr;
      Rel_Action : Action_Func_T) return Btn.Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Make a button selected
   --  @param self pointer to a list object
   --  @param btn pointer to a button to select
    procedure Set_Btn_Selected (Self : Instance; Btn : Obj_T);

   --  Set scroll animation duration on 'list_up()' 'list_down()' 'list_focus()'
   --  @param self pointer to a list object
   --  @param anim_time duration of animation [ms]
   procedure Set_Anim_Time (Self : Instance; Anim_Time : Uint16_T);

   --  Set the scroll bar mode of a list
   --  @param self pointer to a list object
   --  @param sb_mode the new mode from 'lv_page_sb_mode_t' enum
   procedure Set_Sb_Mode (Self : Instance; Mode : Lv.Objx.Page.Mode_T);

   --  Set a style of a list
   --  @param self pointer to a list object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the text of a list element
   --  @param btn pointer to list element
   --  @return pointer to the text
   function Btn_Text
     (Self : Instance) return C_String_Ptr;

   --  Get the label object from a list element
   --  @param self pointer to a list element (button)
   --  @return pointer to the label from the list element or NULL if not found
   function Btn_Label (Self : Instance) return Label.Instance;

   --  Get the image object from a list element
   --  @param self pointer to a list element (button)
   --  @return pointer to the image from the list element or NULL if not found
   function Btn_Img (Self : Instance) return Obj_T;

   --  Get the next button from list. (Starts from the bottom button)
   --  @param self pointer to a list object
   --  @param prev_btn pointer to button. Search the next after it.
   --  @return pointer to the next button or NULL when no more buttons
   function Prev_Btn (Self : Instance; Prev : Obj_T) return Obj_T;

   --  Get the previous button from list. (Starts from the top button)
   --  @param self pointer to a list object
   --  @param prev_btn pointer to button. Search the previous before it.
   --  @return pointer to the previous button or NULL when no more buttons
   function Next_Btn (Self : Instance; Next : Obj_T) return Obj_T;

   --  Get the currently selected button
   --  @param self pointer to a list object
   --  @return pointer to the selected button
   function Btn_Selected (Self : Instance) return Obj_T;

   --  Get scroll animation duration
   --  @param self pointer to a list object
   --  @return duration of animation [ms]
   function Anim_Time (Self : Instance) return Uint16_T;

   --  Get the scroll bar mode of a list
   --  @param self pointer to a list object
   --  @return scrollbar mode from 'lv_page_sb_mode_t' enum
   function Sb_Mode (Self : Instance) return Lv.Objx.Page.Mode_T;

   --  Get a style of a list
   --  @param self pointer to a list object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   --  Other functions
   --  Move the list elements up by one
   --  @param self pointer a to list object
   procedure Up (Self : Instance);

   --  Move the list elements down by one
   --  @param self pointer to a list object
   procedure Down (Self : Instance);

   --  Focus on a list button. It ensures that the button will be visible on the list.
   --  @param self pointer to a list button to focus
   --  @param anim_en true: scroll with animation, false: without animation
   procedure Focus (Self : Instance; Anim_En : U_Bool);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_list_create");
   pragma Import (C, Clean, "lv_list_clean");
   pragma Import (C, Add, "lv_list_add");
   pragma Import (C, Set_Btn_Selected, "lv_list_set_btn_selected");
   pragma Import (C, Set_Anim_Time, "lv_list_set_anim_time");
   pragma Import (C, Set_Sb_Mode, "lv_list_set_sb_mode_inline");
   pragma Import (C, Set_Style, "lv_list_set_style");
   pragma Import (C, Btn_Text, "lv_list_get_btn_text");
   pragma Import (C, Btn_Label, "lv_list_get_btn_label");
   pragma Import (C, Btn_Img, "lv_list_get_btn_img");
   pragma Import (C, Prev_Btn, "lv_list_get_prev_btn");
   pragma Import (C, Next_Btn, "lv_list_get_next_btn");
   pragma Import (C, Btn_Selected, "lv_list_get_btn_selected");
   pragma Import (C, Anim_Time, "lv_list_get_anim_time");
   pragma Import (C, Sb_Mode, "lv_list_get_sb_mode_inline");
   pragma Import (C, Style, "lv_list_get_style");
   pragma Import (C, Up, "lv_list_up");
   pragma Import (C, Down, "lv_list_down");
   pragma Import (C, Focus, "lv_list_focus");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg          => 0,
                    Style_Scrl        => 1,
                    Style_Sb          => 2,
                    Style_Btn_Rel     => 3,
                    Style_Btn_Pr      => 4,
                    Style_Btn_Tgl_Rel => 5,
                    Style_Btn_Tgl_Pr  => 6,
                    Style_Btn_Ina     => 7);

end Lv.Objx.List;
