with System;
with Lv.Style;

with Lv.Objx.Textarea;

package Lv.Objx.Keyboard is

   subtype Instance is Obj_T;

   type Mode_T is (Mode_Text, Mode_Num);

   type Style_T is
     (Style_Bg,
      Style_Btn_Rel,
      Style_Btn_Pr,
      Style_Btn_Tgl_Rel,
      Style_Btn_Tgl_Pr,
      Style_Btn_Ina);

   --  Create a keyboard objects
   --  @param par pointer to an object, it will be the parent of the new keyboard
   --  @param copy pointer to a keyboard object, if not NULL then the new object will be copied from it
   --  @return pointer to the created keyboard
   function Create (Par : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Assign a Text Area to the Keyboard. The pressed characters will be put there.
   --  @param self pointer to a Keyboard object
   --  @param ta pointer to a Text Area object to write there
   procedure Set_Textarea (Self : Instance; Ta : Textarea.Instance);

   --  Set a new a mode (text or number map)
   --  @param self pointer to a Keyboard object
   --  @param mode the mode from 'lv_kb_mode_t'
   procedure Set_Mode (Self : Instance; Mode : Mode_T);

   --  Automatically hide or show the cursor of the current Text Area
   --  @param self pointer to a Keyboard object
   --  @param en true: show cursor on the current text area, false: hide cursor
   procedure Set_Cursor_Manage (Self : Instance; En : U_Bool);

   --  Set call back to call when the "Ok" button is pressed
   --  @param self pointer to Keyboard object
   --  @param action a callback with 'lv_action_t' type
   procedure Set_Ok_Action (Self : Instance; Action : Action_Func_T);

   --  Set call back to call when the "Hide" button is pressed
   --  @param self pointer to Keyboard object
   --  @param action a callback with 'lv_action_t' type
   procedure Set_Hide_Action (Self : Instance; Action : Action_Func_T);

   --  Set a new map for the keyboard
   --  @param self pointer to a Keyboard object
   --  @param map pointer to a string array to describe the map.
   --             See 'lv_btnm_set_map()' for more info.
   procedure Set_Map (Self : Instance; Map : System.Address);

   --  Set a style of a keyboard
   --  @param self pointer to a keyboard object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Assign a Text Area to the Keyboard. The pressed characters will be put there.
   --  @param self pointer to a Keyboard object
   --  @return pointer to the assigned Text Area object
   function Textarea (Self : Instance) return Textarea.Instance;

   --  Set a new a mode (text or number map)
   --  @param self pointer to a Keyboard object
   --  @return the current mode from 'lv_kb_mode_t'
   function Mode (Self : Instance) return Mode_T;

   --  Get the current cursor manage mode.
   --  @param self pointer to a Keyboard object
   --  @return true: show cursor on the current text area, false: hide cursor
   function Cursor_Manage (Self : Instance) return U_Bool;

   --  Get the callback to call when the "Ok" button is pressed
   --  @param self pointer to Keyboard object
   --  @return the ok callback
   function Ok_Action (Self : Instance) return Action_Func_T;

   --  Get the callback to call when the "Hide" button is pressed
   --  @param self pointer to Keyboard object
   --  @return the close callback
   function Hide_Action (Self : Instance) return Action_Func_T;

   --  Get a style of a keyboard
   --  @param self pointer to a keyboard object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_kb_create");
   pragma Import (C, Set_Textarea, "lv_kb_set_ta");
   pragma Import (C, Set_Mode, "lv_kb_set_mode");
   pragma Import (C, Set_Cursor_Manage, "lv_kb_set_cursor_manage");
   pragma Import (C, Set_Ok_Action, "lv_kb_set_ok_action");
   pragma Import (C, Set_Hide_Action, "lv_kb_set_hide_action");
   pragma Import (C, Set_Map, "lv_kb_set_map_inline");
   pragma Import (C, Set_Style, "lv_kb_set_style");
   pragma Import (C, Textarea, "lv_kb_get_ta");
   pragma Import (C, Mode, "lv_kb_get_mode");
   pragma Import (C, Cursor_Manage, "lv_kb_get_cursor_manage");
   pragma Import (C, Ok_Action, "lv_kb_get_ok_action");
   pragma Import (C, Hide_Action, "lv_kb_get_hide_action");
   pragma Import (C, Style, "lv_kb_get_style");

   for Mode_T'Size use 8;
   for Mode_T use (Mode_Text => 0, Mode_Num => 1);

   for Style_T'Size use 8;
   for Style_T use
     (Style_Bg          => 0,
      Style_Btn_Rel     => 1,
      Style_Btn_Pr      => 2,
      Style_Btn_Tgl_Rel => 3,
      Style_Btn_Tgl_Pr  => 4,
      Style_Btn_Ina     => 5);

end Lv.Objx.Keyboard;
