with Lv.Style;

package Lv.Objx.Switch is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg,
                    Style_Indic,
                    Style_Knob_Off,
                    Style_Knob_On);

   --  Create a switch objects
   --  @param par pointer to an object, it will be the parent of the new switch
   --  @param copy pointer to a switch object, if not NULL then the new object will be copied from it
   --  @return pointer to the created switch
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Turn ON the switch
   --  @param self pointer to a switch object
   procedure On (Self : Instance);

   --  Turn OFF the switch
   --  @param self pointer to a switch object
   procedure Off (Self : Instance);

   --  Set a function which will be called when the switch is toggled by the user
   --  @param self pointer to switch object
   --  @param action a callback function
   procedure Set_Action (Self : Instance; Action : Action_Func_T);

   --  Set a style of a switch
   --  @param self pointer to a switch object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the state of a switch
   --  @param self pointer to a switch object
   --  @return false: OFF; true: ON
   function State (Self : Instance) return U_Bool;

   --  Get the switch action function
   --  @param self pointer to a switch object
   --  @return the callback function
   function Action (Self : Instance) return Action_Func_T;

   --  Get a style of a switch
   --  @param self pointer to a  switch object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self : Instance;
      Arg2 : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_sw_create");
   pragma Import (C, On, "lv_sw_on");
   pragma Import (C, Off, "lv_sw_off");
   pragma Import (C, Set_Action, "lv_sw_set_action_inline");
   pragma Import (C, Set_Style, "lv_sw_set_style");
   pragma Import (C, State, "lv_sw_get_state_inline");
   pragma Import (C, Action, "lv_sw_get_action_inline");
   pragma Import (C, Style, "lv_sw_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg       => 0,
                    Style_Indic    => 1,
                    Style_Knob_Off => 2,
                    Style_Knob_On  => 3);

end Lv.Objx.Switch;
