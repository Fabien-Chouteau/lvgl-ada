with System;
with Lv.Style;
with Lv.Objx.Btn;

package Lv.Objx.Imgbtn is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg,
                    Style_Pr,
                    Style_Tgl_Rel,
                    Style_Tgl_Pr,
                    Style_Ina);

   --  Create a image button objects
   --  @param par pointer to an object, it will be the parent of the new image button
   --  @param copy pointer to a image button object, if not NULL then the new object will be copied from it
   --  @return pointer to the created image button
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set images for a state of the image button
   --  @param imgbtn pointer to an image button object
   --  @param state for which state set the new image (from `lv_btn_state_t`) `
   --  @param src pointer to an image source (a C array or path to a file)
    procedure Set_Src
     (Self  : Instance;
      State : Lv.Objx.Btn.State_T;
      Src   : System.Address);

   --  Enable the toggled states. On release the button will change from/to toggled state.
   --  @param imgbtn pointer to an image button object
   --  @param tgl true: enable toggled states, false: disable
   procedure Set_Toggle (Self : Instance; Tgl : u_Bool);

   --  Set the state of the image button
   --  @param imgbtn pointer to an image button object
   --  @param state the new state of the button (from lv_btn_state_t enum)
   procedure Set_State (Self : Instance; State : Lv.Objx.Btn.State_T);

   --  Toggle the state of the image button (ON->OFF, OFF->ON)
   --  @param imgbtn pointer to a image button object
   procedure Toggle (Self : Instance);

   --  Set a function to call when a button event happens
   --  @param imgbtn pointer to an image button object
   --  @param action type of event form 'lv_action_t' (press, release, long press, long press repeat)
   procedure Set_Action
     (Self   : Instance;
      Type_P : Lv.Objx.Btn.Action_T;
      Action : Lv.Objx.Action_Func_T);

   --  Set a style of a image button.
   --  @param imgbtn pointer to image button object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : style_t;
      Style  : LV.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the images in a  given state
   --  @param imgbtn pointer to an image button object
   --  @param state the state where to get the image (from `lv_btn_state_t`) `
   --  @return pointer to an image source (a C array or path to a file)
   function Src (Self : Instance; State : LV.Objx.Btn.State_T) return System.Address;

   --  Get the current state of the image button
   --  @param imgbtn pointer to a image button object
   --  @return the state of the button (from lv_btn_state_t enum)
   function State (Self : Instance) return Lv.Objx.Btn.State_T;

   --  Get the toggle enable attribute of the image button
   --  @param imgbtn pointer to a image button object
   --  @return ture: toggle enabled, false: disabled
   function Toggled (Self : Instance) return u_Bool;

   --  Get the release action of a image button
   --  @param imgbtn pointer to a image button object
   --  @return pointer to the release action function
   function Action (Self : Instance; Type_P : LV.Objx.Btn.Action_T) return Lv.Objx.Action_Func_T;

   --  Get style of a image button.
   --  @param imgbtn pointer to image button object
   --  @param type which style should be get
   --  @return style pointer to the style
   function Style (Self : Instance; Type_P : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_imgbtn_create");
   pragma Import (C, Set_Src, "lv_imgbtn_set_src");
   pragma Import (C, Set_Toggle, "lv_imgbtn_set_toggle");
   pragma Import (C, Set_State, "lv_imgbtn_set_state");
   pragma Import (C, Toggle, "lv_imgbtn_toggle");
   pragma Import (C, Set_Action, "lv_imgbtn_set_action");
   pragma Import (C, Set_Style, "lv_imgbtn_set_style");
   pragma Import (C, Src, "lv_imgbtn_get_src");
   pragma Import (C, State, "lv_imgbtn_get_state");
   pragma Import (C, Toggled, "lv_imgbtn_get_toggle");
   pragma Import (C, Action, "lv_imgbtn_get_action");
   pragma Import (C, Style, "lv_imgbtn_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg      => 0,
                    Style_Pr      => 1,
                    Style_Tgl_Rel => 2,
                    Style_Tgl_Pr  => 3,
                    Style_Ina     => 4);

end Lv.Objx.imgbtn;
