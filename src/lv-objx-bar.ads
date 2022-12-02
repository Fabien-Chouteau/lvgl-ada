with Lv.Style;

package Lv.Objx.Bar is

   subtype Instance is Obj_T;

   type Style_T is (Style_Bg, Style_Indic);

   --  Create a bar objects
   --  @param par pointer to an object, it will be the parent of the new bar
   --  @param copy pointer to a bar object, if not NULL then the new object will be copied from it
   --  @return pointer to the created bar
   function Create (Parent : Instance; Copy : Obj_T) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set a new value on the bar
   --  @param self pointer to a bar object
   --  @param value new value
   procedure Set_Value (Self : Instance; Arg2 : Int16_T);

   --  Set a new value with animation on the bar
   --  @param self pointer to a bar object
   --  @param value new value
   --  @param anim_time animation time in milliseconds
   procedure Set_Value_Anim (Self : Instance; Arg2 : Int16_T; Arg3 : Uint16_T);

   --  Set minimum and the maximum values of a bar
   --  @param self pointer to the bar object
   --  @param min minimum value
   --  @param max maximum value
   procedure Set_Range (Self : Instance; Arg2 : Int16_T; Arg3 : Int16_T);

   --  Set a style of a bar
   --  @param self pointer to a bar object
   --  @param type which style should be set
   --  @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the value of a bar
   --  @param self pointer to a bar object
   --  @return the value of the bar
   function Value (Self : Instance) return Int16_T;

   --  Get the minimum value of a bar
   --  @param self pointer to a bar object
   --  @return the minimum value of the bar
   function Min_Value (Self : Instance) return Int16_T;

   --  Get the maximum value of a bar
   --  @param self pointer to a bar object
   --  @return the maximum value of the bar
   function Max_Value (Self : Instance) return Int16_T;

   --  Get a style of a bar
   --  @param self pointer to a bar object
   --  @param type which style should be get
   --  @return style pointer to a style
   function Style (Self : Instance; Arg2 : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_bar_create");
   pragma Import (C, Set_Value, "lv_bar_set_value");
   pragma Import (C, Set_Value_Anim, "lv_bar_set_value_anim");
   pragma Import (C, Set_Range, "lv_bar_set_range");
   pragma Import (C, Set_Style, "lv_bar_set_style");
   pragma Import (C, Value, "lv_bar_get_value");
   pragma Import (C, Min_Value, "lv_bar_get_min_value");
   pragma Import (C, Max_Value, "lv_bar_get_max_value");
   pragma Import (C, Style, "lv_bar_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Bg    => 0,
                    Style_Indic => 1);

end Lv.Objx.Bar;
