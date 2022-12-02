with Lv.Color;
with Lv.Style;

package Lv.Objx.Gauge is

   subtype Instance is Obj_T;

   --  Create a gauge objects
   --  @param par pointer to an object, it will be the parent of the new gauge
   --  @param copy pointer to a gauge object, if not NULL then the new object will be copied from it
   --  @return pointer to the created gauge
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the number of needles
   --  @param self pointer to gauge object
   --  @param needle_cnt new count of needles
   --  @param colors an array of colors for needles (with 'num' elements)
   procedure Set_Needle_Count
     (Self       : Instance;
      Needle_Cnt : Uint8_T;
      Colors     : access constant Lv.Color.Color_T);

   --  Set the value of a needle
   --  @param self pointer to a gauge
   --  @param needle_id the id of the needle
   --  @param value the new value
   procedure Set_Value (Self : Instance; Needle_Id : Uint8_T; Value : Int16_T);

   --  Set minimum and the maximum values of a gauge
   --  @param self pointer to he gauge object
   --  @param min minimum value
   --  @param max maximum value
   procedure Set_Range (Self : Instance; Min : Int16_T; Max : Int16_T);

   --  Set a critical value on the scale. After this value 'line.color' scale lines will be drawn
   --  @param self pointer to a gauge object
   --  @param value the critical value
   procedure Set_Critical_Value (Self : Instance; Value : Int16_T);

   --  Set the scale settings of a gauge
   --  @param self pointer to a gauge object
   --  @param angle angle of the scale (0..360)
   --  @param line_cnt count of scale lines.
   --  The get a given "subdivision" lines between label, `line_cnt` = (sub_div + 1)   --  (label_cnt - 1) + 1
   --  @param label_cnt count of scale labels.
   procedure Set_Scale
     (Self      : Instance;
      Angle     : Uint16_T;
      Line_Cnt  : Uint8_T;
      Label_Cnt : Uint8_T);

   --  Set the styles of a gauge
   --  @param self pointer to a gauge object
   --  @param bg set the style of the gauge
   procedure Set_Style (Self : Instance; Bg : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the value of a needle
   --  @param self pointer to gauge object
   --  @param needle the id of the needle
   --  @return the value of the needle [min,max]
   function Value (Self : Instance; Needle : Uint8_T) return Int16_T;

   --  Get the count of needles on a gauge
   --  @param self pointer to gauge
   --  @return count of needles
   function Needle_Count (Self : Instance) return Uint8_T;

   --  Get the minimum value of a gauge
   --  @param self pointer to a gauge object
   --  @return the minimum value of the gauge
   function Min_Value (Self : Instance) return Int16_T;

   --  Get the maximum value of a gauge
   --  @param self pointer to a gauge object
   --  @return the maximum value of the gauge
   function Max_Value (Self : Instance) return Int16_T;

   --  Get a critical value on the scale.
   --  @param self pointer to a gauge object
   --  @return the critical value
   function Critical_Value (Self : Instance) return Int16_T;

   --  Set the number of labels (and the thicker lines too)
   --  @param self pointer to a gauge object
   --  @return count of labels
   function Label_Count (Self : Instance) return Uint8_T;

   --  Get the scale number of a gauge
   --  @param self pointer to a gauge object
   --  @return number of the scale units
   function Line_Count (Self : Instance) return Uint8_T;

   --  Get the scale angle of a gauge
   --  @param self pointer to a gauge object
   --  @return angle of the scale
   function Scale_Angle (Self : Instance) return Uint16_T;

   --  Get the style of a gauge
   --  @param self pointer to a gauge object
   --  @return pointer to the gauge's style
   function Style (Self : Instance) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_gauge_create");
   pragma Import (C, Set_Needle_Count, "lv_gauge_set_needle_count");
   pragma Import (C, Set_Value, "lv_gauge_set_value");
   pragma Import (C, Set_Range, "lv_gauge_set_range_inline");
   pragma Import (C, Set_Critical_Value, "lv_gauge_set_critical_value_inline");
   pragma Import (C, Set_Scale, "lv_gauge_set_scale");
   pragma Import (C, Set_Style, "lv_gauge_set_style_inline");
   pragma Import (C, Value, "lv_gauge_get_value");
   pragma Import (C, Needle_Count, "lv_gauge_get_needle_count");
   pragma Import (C, Min_Value, "lv_gauge_get_min_value_inline");
   pragma Import (C, Max_Value, "lv_gauge_get_max_value_inline");
   pragma Import (C, Critical_Value, "lv_gauge_get_critical_value_inline");
   pragma Import (C, Label_Count, "lv_gauge_get_label_count");
   pragma Import (C, Line_Count, "lv_gauge_get_line_count_inline");
   pragma Import (C, Scale_Angle, "lv_gauge_get_scale_angle_inline");
   pragma Import (C, Style, "lv_gauge_get_style_inline");

end Lv.Objx.Gauge;
