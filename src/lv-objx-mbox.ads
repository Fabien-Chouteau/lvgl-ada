with System;
with Lv.Objx.Btnm;
with Lv.Style;

package Lv.Objx.Mbox is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg,
                    Style_Btn_Bg,
                    Style_Btn_Rel,
                    Style_Btn_Pr,
                    Style_Btn_Tgl_Rel,
                    Style_Btn_Tgl_Pr,
                    Style_Btn_Ina);

   --  Create a message box objects
   --  @param par pointer to an object, it will be the parent of the new message box
   --  @param copy pointer to a message box object, if not NULL then the new object will be copied from it
   --  @return pointer to the created message box
   function Create (Par : Obj_T; Copy : Obj_T) return Instance;

   --  Add button to the message box
   --  @param self pointer to message box object
   --  @param btn_map button descriptor (button matrix map).
   --                 E.g.  a const char   -- txt[] = {"ok", "close", ""} (Can not be local variable)
   --  @param action a function which will be called when a button is released
   procedure Add_Btns
     (Self    : Instance;
      Btn_Map : String_Array_Ptr;
      Action  : Lv.Objx.Btnm.Action_T);

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the text of the message box
   --  @param self pointer to a message box
   --  @param txt a '\0' terminated character string which will be the message box text
   procedure Set_Text (Self : Instance; Arg2 : C_String_Ptr);

   --  Stop the action to call when button is released
   --  @param self pointer to a message box object
   --  @param pointer to an 'lv_btnm_action_t' action. In the action you need to use `lv_mbox_get_from_btn()` to get the `mbox`.
   procedure Set_Action (Self : Instance; Arg2 : Lv.Objx.Btnm.Action_T);

   --  Set animation duration
   --  @param self pointer to a message box object
   --  @param anim_time animation length in  milliseconds (0: no animation)
   procedure Set_Anim_Time (Self : Instance; Arg2 : Uint16_T);

   --  Automatically delete the message box after a given time
   --  @param self pointer to a message box object
   --  @param delay a time (in milliseconds) to wait before delete the message box
   procedure Start_Auto_Close (Self : Instance; Arg2 : Uint16_T);

   --  Stop the auto. closing of message box
   --  @param self pointer to a message box object
   procedure Stop_Auto_Close (Self : Instance);

   --  Set a style of a message box
   --  @param self pointer to a message box object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the text of the message box
   --  @param self pointer to a message box object
   --  @return pointer to the text of the message box
   function Text (Self : Instance) return C_String_Ptr;

   --  Get the message box object from one of its button.
   --  It is useful in the button release actions where only the button is known
   --  @param btn pointer to a button of a message box
   --  @return pointer to the button's message box
   function From_Btn (Button : Obj_T) return Instance;

   --  Get the animation duration (close animation time)
   --  @param self pointer to a message box object
   --  @return animation length in  milliseconds (0: no animation)
   function Anim_Time (Self : Instance) return Uint16_T;

   --  Get a style of a message box
   --  @param self pointer to a message box object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style (Self : Instance; Arg2 : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_mbox_create");
   pragma Import (C, Add_Btns, "lv_mbox_add_btns");
   pragma Import (C, Set_Text, "lv_mbox_set_text");
   pragma Import (C, Set_Action, "lv_mbox_set_action");
   pragma Import (C, Set_Anim_Time, "lv_mbox_set_anim_time");
   pragma Import (C, Start_Auto_Close, "lv_mbox_start_auto_close");
   pragma Import (C, Stop_Auto_Close, "lv_mbox_stop_auto_close");
   pragma Import (C, Set_Style, "lv_mbox_set_style");
   pragma Import (C, Text, "lv_mbox_get_text");
   pragma Import (C, From_Btn, "lv_mbox_get_from_btn");
   pragma Import (C, Anim_Time, "lv_mbox_get_anim_time");
   pragma Import (C, Style, "lv_mbox_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg          => 0,
                    Style_Btn_Bg      => 1,
                    Style_Btn_Rel     => 2,
                    Style_Btn_Pr      => 3,
                    Style_Btn_Tgl_Rel => 4,
                    Style_Btn_Tgl_Pr  => 5,
                    Style_Btn_Ina     => 6);

end Lv.Objx.Mbox;
