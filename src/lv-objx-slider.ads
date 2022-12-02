with Lv.Style;

package Lv.Objx.Slider is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg,
                    Style_Indic,
                    Style_Knob);

   --  Create a slider objects
   --  @param par pointer to an object, it will be the parent of the new slider
   --  @param copy pointer to a slider object, if not NULL then the new object will be copied from it
   --  @return pointer to the created slider
   function Create (Par : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set a new value on the slider
   --  @param self pointer to a slider object
   --  @param value new value
   procedure Set_Value (Self : Instance; Value : Int16_T);

   --  Set a new value with animation on a slider
   --  @param self pointer to a slider object
   --  @param value new value
   --  @param anim_time animation time in milliseconds
   procedure Set_Value_Anim
     (Self      : Instance;
      Value     : Int16_T;
      Anim_Time : Uint16_T);

   --  Set minimum and the maximum values of a bar
   --  @param self pointer to the slider object
   --  @param min minimum value
   --  @param max maximum value
   procedure Set_Range (Self : Instance; Min : Int16_T; Max : Int16_T);

   --  Set a function which will be called when a new value is set on the slider
   --  @param self pointer to slider object
   --  @param action a callback function
   procedure Set_Action (Self : Instance; Action : Action_Func_T);

   --  Set the 'knob in' attribute of a slider
   --  @param self pointer to slider object
   --  @param in true: the knob is drawn always in the slider;
   --            false: the knob can be out on the edges
   procedure Set_Knob_In (Self : Instance; In_P : U_Bool);

   --  Set a style of a slider
   --  @param self pointer to a slider object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the value of a slider
   --  @param self pointer to a slider object
   --  @return the value of the slider
   function Value (Self : Instance) return Int16_T;

   --  Get the minimum value of a slider
   --  @param self pointer to a slider object
   --  @return the minimum value of the slider
   function Min_Value (Self : Instance) return Int16_T;

   --  Get the maximum value of a slider
   --  @param self pointer to a slider object
   --  @return the maximum value of the slider
   function Max_Value (Self : Instance) return Int16_T;

   --  Get the slider action function
   --  @param self pointer to slider object
   --  @return the callback function
   function Action (Self : Instance) return Action_Func_T;

   --  Give the slider is being dragged or not
   --  @param self pointer to a slider object
   --  @return true: drag in progress false: not dragged
   function Is_Dragged (Self : Instance) return U_Bool;

   --  Get the 'knob in' attribute of a slider
   --  @param self pointer to slider object
   --  @return true: the knob is drawn always in the slider;
   --          false: the knob can be out on the edges
   function Knob_In (Self : Instance) return U_Bool;

   --  Get a style of a slider
   --  @param self pointer to a slider object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style
     (Self   : Instance;
      Type_P : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_slider_create");
   pragma Import (C, Set_Value, "lv_slider_set_value_inline");
   pragma Import (C, Set_Value_Anim, "lv_slider_set_value_anim_inline");
   pragma Import (C, Set_Range, "lv_slider_set_range_inline");
   pragma Import (C, Set_Action, "lv_slider_set_action");
   pragma Import (C, Set_Knob_In, "lv_slider_set_knob_in");
   pragma Import (C, Set_Style, "lv_slider_set_style");
   pragma Import (C, Value, "lv_slider_get_value");
   pragma Import (C, Min_Value, "lv_slider_get_min_value_inline");
   pragma Import (C, Max_Value, "lv_slider_get_max_value_inline");
   pragma Import (C, Action, "lv_slider_get_action");
   pragma Import (C, Is_Dragged, "lv_slider_is_dragged");
   pragma Import (C, Knob_In, "lv_slider_get_knob_in");
   pragma Import (C, Style, "lv_slider_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg    => 0,
                    Style_Indic => 1,
                    Style_Knob  => 2);

end Lv.Objx.Slider;
