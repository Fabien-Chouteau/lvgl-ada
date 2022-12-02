with Lv.Style;
with Lv.Objx.Cont;

package Lv.Objx.Btn is

   subtype Instance is Obj_T;

   type State_T is (State_Rel,
                    State_Pr,
                    State_Tgl_Rel,
                    State_Tgl_Pr,
                    State_Ina,
                    State_Num);

   type Action_T is (Action_Click,
                     Action_Pr,
                     Action_Long_Pr,
                     Action_Long_Pr_Repeat,
                     Action_Num);

   type Style_T is (Style_Rel,
                    Style_Pr,
                    Style_Tgl_Rel,
                    Style_Tgl_Pr,
                    Style_Ina);

   --  Create a button objects
   --  @param par pointer to an object, it will be the parent of the new button
   --  @param copy pointer to a button object, if not NULL then the new object will be copied from it
   --  @return pointer to the created button
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Enable the toggled states. On release the button will change from/to toggled state.
   --  @param self pointer to a button object
   --  @param tgl true: enable toggled states, false: disable
   procedure Set_Toggle (Self : Instance; Tgl : U_Bool);

   --  Set the state of the button
   --  @param self pointer to a button object
   --  @param state the new state of the button (from lv_btn_state_t enum)
   procedure Set_State (Self : Instance; State : State_T);

   --  Toggle the state of the button (ON->OFF, OFF->ON)
   --  @param self pointer to a button object
   procedure Toggle (Self : Instance);

   --  Set a function to call when a button event happens
   --  @param self pointer to a button object
   --  @param action type of event form 'lv_action_t' (press, release, long press, long press repeat)
   procedure Set_Action (Self : Instance; Type_P : Action_T; Action : Action_Func_T);

   --  Set the layout on a button
   --  @param self pointer to a button object
   --  @param layout a layout from 'lv_cont_layout_t'
   procedure Set_Layout (Self : Instance; Layout : Lv.Objx.Cont.Layout_T);

   --  Enable the horizontal or vertical fit.
   --  The button size will be set to involve the children horizontally or vertically.
   --  @param self pointer to a button object
   --  @param hor_en true: enable the horizontal fit
   --  @param ver_en true: enable the vertical fit
   procedure Set_Fit (Self : Instance; Hor_En : U_Bool; Ver_En : U_Bool);

   --  Set time of the ink effect (draw a circle on click to animate in the new state)
   --  @param self pointer to a button object
   --  @param time the time of the ink animation
   procedure Set_Ink_In_Time (Self : Instance; Time : Uint16_T);

   --  Set the wait time before the ink disappears
   --  @param self pointer to a button object
   --  @param time the time of the ink animation
   procedure Set_Ink_Wait_Time (Self : Instance; Time : Uint16_T);

   --  Set time of the ink out effect (animate to the released state)
   --  @param self pointer to a button object
   --  @param time the time of the ink animation
   procedure Set_Ink_Out_Time (Self : Instance; Time : Uint16_T);

   --  Set a style of a button.
   --  @param self pointer to button object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the current state of the button
   --  @param self pointer to a button object
   --  @return the state of the button (from lv_btn_state_t enum)
   function State (Self : Instance) return State_T;

   --  Get the toggle enable attribute of the button
   --  @param self pointer to a button object
   --  @return ture: toggle enabled, false: disabled
   function Toggle_En (Self : Instance) return U_Bool;

   --  Get the release action of a button
   --  @param self pointer to a button object
   --  @return pointer to the release action function
   function Action (Self : Instance; Action : Action_T) return Action_Func_T;

   --  Get the layout of a button
   --  @param self pointer to button object
   --  @return the layout from 'lv_cont_layout_t'
   function Layout (Self : Instance) return Lv.Objx.Cont.Layout_T;

   --  Get horizontal fit enable attribute of a button
   --  @param self pointer to a button object
   --  @return true: horizontal fit is enabled; false: disabled
   function Hor_Fit (Self : Instance) return U_Bool;

   --  Get vertical fit enable attribute of a container
   --  @param self pointer to a button object
   --  @return true: vertical fit is enabled; false: disabled
   function Ver_Fit (Self : Instance) return U_Bool;

   --  Get time of the ink in effect (draw a circle on click to animate in the new state)
   --  @param self pointer to a button object
   --  @return the time of the ink animation
   function Ink_In_Time (Self : Instance) return Uint16_T;

   --  Get the wait time before the ink disappears
   --  @param self pointer to a button object
   --  @return the time of the ink animation
   function Ink_Wait_Time (Self : Instance) return Uint16_T;

   --  Get time of the ink out effect (animate to the releases state)
   --  @param self pointer to a button object
   --  @return the time of the ink animation
   function Ink_Out_Time (Self : Instance) return Uint16_T;

   --  Get style of a button.
   --  @param self pointer to button object
   --  @param type which style should be get
   --  @return style pointer to the style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_btn_create");
   pragma Import (C, Set_Toggle, "lv_btn_set_toggle");
   pragma Import (C, Set_State, "lv_btn_set_state");
   pragma Import (C, Toggle, "lv_btn_toggle");
   pragma Import (C, Set_Action, "lv_btn_set_action");
   pragma Import (C, Set_Layout, "lv_btn_set_layout_inline");
   pragma Import (C, Set_Fit, "lv_btn_set_fit_inline");
   pragma Import (C, Set_Ink_In_Time, "lv_btn_set_ink_in_time");
   pragma Import (C, Set_Ink_Wait_Time, "lv_btn_set_ink_wait_time");
   pragma Import (C, Set_Ink_Out_Time, "lv_btn_set_ink_out_time");
   pragma Import (C, Set_Style, "lv_btn_set_style");
   pragma Import (C, State, "lv_btn_get_state");
   pragma Import (C, Toggle_En, "lv_btn_get_toggle");
   pragma Import (C, Action, "lv_btn_get_action");
   pragma Import (C, Layout, "lv_btn_get_layout_inline");
   pragma Import (C, Hor_Fit, "lv_btn_get_hor_fit_inline");
   pragma Import (C, Ver_Fit, "lv_btn_get_ver_fit_inline");
   pragma Import (C, Ink_In_Time, "lv_btn_get_ink_in_time");
   pragma Import (C, Ink_Wait_Time, "lv_btn_get_ink_wait_time");
   pragma Import (C, Ink_Out_Time, "lv_btn_get_ink_out_time");
   pragma Import (C, Style, "lv_btn_get_style");

   for State_T'Size use 8;
   for State_T use
     (State_Rel     => 0,
      State_Pr      => 1,
      State_Tgl_Rel => 2,
      State_Tgl_Pr  => 3,
      State_Ina     => 4,
      State_Num     => 5);

   for Action_T'Size use 8;
   for Action_T use
     (Action_Click          => 0,
      Action_Pr             => 1,
      Action_Long_Pr        => 2,
      Action_Long_Pr_Repeat => 3,
      Action_Num            => 4);

   for Style_T'Size use 8;
   for Style_T use (Style_Rel     => 0,
                    Style_Pr      => 1,
                    Style_Tgl_Rel => 2,
                    Style_Tgl_Pr  => 3,
                    Style_Ina     => 4);

end Lv.Objx.Btn;
