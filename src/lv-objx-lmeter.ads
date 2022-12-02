with Lv.Style;

package Lv.Objx.Lmeter is

   subtype Instance is Obj_T;

   --  Create a line meter objects
   --  @param par pointer to an object, it will be the parent of the new line meter
   --  @param copy pointer to a line meter object, if not NULL then the new object will be copied from it
   --  @return pointer to the created line meter
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set a new value on the line meter
   --  @param self pointer to a line meter object
   --  @param value new value
   procedure Set_Value (Self : Instance; Value : Int16_T);

   --  Set minimum and the maximum values of a line meter
   --  @param self pointer to he line meter object
   --  @param min minimum value
   --  @param max maximum value
   procedure Set_Range (Self : Instance; Min : Int16_T; Max : Int16_T);

   --  Set the scale settings of a line meter
   --  @param self pointer to a line meter object
   --  @param angle angle of the scale (0..360)
   --  @param line_cnt number of lines
   procedure Set_Scale (Self : Instance; Angle : Uint16_T; Line_Cnt : Uint8_T);

   --  Set the styles of a line meter
   --  @param self pointer to a line meter object
   --  @param bg set the style of the line meter
   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the value of a line meter
   --  @param self pointer to a line meter object
   --  @return the value of the line meter
   function Value (Self : Instance) return Int16_T;

   --  Get the minimum value of a line meter
   --  @param self pointer to a line meter object
   --  @return the minimum value of the line meter
   function Min_Value (Self : Instance) return Int16_T;

   --  Get the maximum value of a line meter
   --  @param self pointer to a line meter object
   --  @return the maximum value of the line meter
   function Max_Value (Self : Instance) return Int16_T;

   --  Get the scale number of a line meter
   --  @param self pointer to a line meter object
   --  @return number of the scale units
   function Line_Count (Self : Instance) return Uint8_T;

   --  Get the scale angle of a line meter
   --  @param self pointer to a line meter object
   --  @return angle of the scale
   function Scale_Angle (Self : Instance) return Uint16_T;

   --  Get the style of a line meter
   --  @param self pointer to a line meter object
   --  @return pointer to the line meter's style
   function Style (Self : Instance) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_lmeter_create");
   pragma Import (C, Set_Value, "lv_lmeter_set_value");
   pragma Import (C, Set_Range, "lv_lmeter_set_range");
   pragma Import (C, Set_Scale, "lv_lmeter_set_scale");
   pragma Import (C, Set_Style, "lv_lmeter_set_style_inline");
   pragma Import (C, Value, "lv_lmeter_get_value");
   pragma Import (C, Min_Value, "lv_lmeter_get_min_value");
   pragma Import (C, Max_Value, "lv_lmeter_get_max_value");
   pragma Import (C, Line_Count, "lv_lmeter_get_line_count");
   pragma Import (C, Scale_Angle, "lv_lmeter_get_scale_angle");
   pragma Import (C, Style, "lv_lmeter_get_style_inline");

end Lv.Objx.Lmeter;
