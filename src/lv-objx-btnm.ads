with Lv.Style;
with System;

package Lv.Objx.Btnm is

   subtype Instance is Obj_T;

   Btnm_Ctrl_Code           : constant := 16#80#;
   Btnm_Ctrl_Mask           : constant := 16#C0#;
   Btnm_Width_Mask          : constant := 16#07#;
   Btnm_Hide_Mask           : constant := 16#08#;
   Btnm_Repeat_Disable_Mask : constant := 16#10#;
   Btnm_Inactive_Mask       : constant := 16#20#;

   Btnm_Pr_None             : constant := 16#FFFF#;

   --  Type of callback function which is called when a button is released or
   --  long pressed on the button matrix.
   --  Parameters: button matrix, text of the released button
   --  return Res_Inv if the button matrix is deleted else Res_Ok
   type Action_T is access function
     (Self : Instance;
      Txt  : C_String_Ptr) return Res_T;
   pragma Convention (C, Action_T);

   type Style_T is (Style_Bg,
                    Style_Rel,
                    Style_Pr,
                    Style_Tgl_Rel,
                    Style_Tgl_Pr,
                    Style_Ina);

   --  Create a button matrix objects
   --  @param par pointer to an object, it will be the parent of the new button matrix
   --  @param copy pointer to a button matrix object, if not NULL then the new object will be copied from it
   --  @return pointer to the created button matrix
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set a new map. Buttons will be created/deleted according to the map.
   --  @param self pointer to a button matrix object
   --  @param map pointer a string array. The last string has to be: "".
   --             Use "\n" to begin a new line.
   --             The first byte can be a control data:
   --              - bit 7: always 1
   --              - bit 6: always 0
   --              - bit 5: inactive (disabled)
   --              - bit 4: no repeat (on long press)
   --              - bit 3: hidden
   --              - bit 2..0: button relative width
   --              Example (practically use octal numbers): "\224abc": "abc" text with 4 width and no long press
   procedure Set_Map (Self : Instance; Map : String_Array_Ptr);

   --  Set a new callback function for the buttons (It will be called when a button is released)
   --  @param self: pointer to button matrix object
   --  @param action pointer to a callback function
   procedure Set_Action (Self : Instance; Action : Action_T);

   --  Enable or disable button toggling
   --  @param self pointer to button matrix object
   --  @param en true: enable toggling; false: disable toggling
   --  @param id index of the currently toggled button (ignored if 'en' == false)
   procedure Set_Toggle (Self : Instance; En : U_Bool; Id : Uint16_T);

   --  Set a style of a button matrix
   --  @param self pointer to a button matrix object
   --  @param type which style should be set
   --  @param style pointer to a style
  procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the current map of a button matrix
   --  @param self pointer to a button matrix object
   --  @return the current map
   function Map (Self : Instance) return System.Address;

   --  Get a the callback function of the buttons on a button matrix
   --  @param self: pointer to button matrix object
   --  @return pointer to the callback function
   function Action (Self : Instance) return Action_T;

   --  Get the toggled button
   --  @param self pointer to button matrix object
   --  @return  index of the currently toggled button (0: if unset)
   function Toggled (Self : Instance) return Uint16_T;

   --  Get a style of a button matrix
   --  @param self pointer to a button matrix object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style (Self : Instance; Type_P : Style_T)
                       return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_btnm_create");
   pragma Import (C, Set_Map, "lv_btnm_set_map");
   pragma Import (C, Set_Action, "lv_btnm_set_action");
   pragma Import (C, Set_Toggle, "lv_btnm_set_toggle");
   pragma Import (C, Set_Style, "lv_btnm_set_style");
   pragma Import (C, Map, "lv_btnm_get_map");
   pragma Import (C, Action, "lv_btnm_get_action");
   pragma Import (C, Toggled, "lv_btnm_get_toggled");
   pragma Import (C, Style, "lv_btnm_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg      => 0,
                    Style_Rel     => 1,
                    Style_Pr      => 2,
                    Style_Tgl_Rel => 3,
                    Style_Tgl_Pr  => 4,
                    Style_Ina     => 5);

end Lv.Objx.Btnm;
