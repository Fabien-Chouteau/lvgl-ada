with Interfaces.C; use Interfaces.C;
with Lv.Color;
with Lv.Style;

package Lv.Objx.Gauge is

   subtype Instance is Obj_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_gauge_create");

   procedure Set_Needle_Count
     (Self : Instance;
      Arg2 : Uint8_T;
      Arg3 : access constant Lv.Color.Color_T);
   pragma Import (C, Set_Needle_Count, "lv_gauge_set_needle_count");

   procedure Set_Value (Self : Instance; Needle_Id : Uint8_T; Value : Int16_T);
   pragma Import (C, Set_Value, "lv_gauge_set_value");

   procedure Set_Range (Self : Instance; Min : Int16_T; Max : Int16_T);
   pragma Import (C, Set_Range, "lv_gauge_set_range_inline");

   procedure Set_Critical_Value (Self : Instance; Value : Int16_T);
   pragma Import (C, Set_Critical_Value, "lv_gauge_set_critical_value_inline");

   procedure Set_Scale
     (Self : Instance;
      Arg2 : Uint16_T;
      Arg3 : Uint8_T;
      Arg4 : Uint8_T);
   pragma Import (C, Set_Scale, "lv_gauge_set_scale");

   procedure Set_Style (Self : Instance; Bg : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_gauge_set_style_inline");

   function Get_Value (Self : Instance; Arg2 : Uint8_T) return Int16_T;
   pragma Import (C, Get_Value, "lv_gauge_get_value");

   function Get_Needle_Count (Self : Instance) return Uint8_T;
   pragma Import (C, Get_Needle_Count, "lv_gauge_get_needle_count");

   function Get_Min_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Min_Value, "lv_gauge_get_min_value_inline");

   function Get_Max_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Max_Value, "lv_gauge_get_max_value_inline");

   function Get_Critical_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Critical_Value, "lv_gauge_get_critical_value_inline");

   function Get_Label_Count (Self : Instance) return Uint8_T;
   pragma Import (C, Get_Label_Count, "lv_gauge_get_label_count");

   function Get_Line_Count (Self : Instance) return Uint8_T;
   pragma Import (C, Get_Line_Count, "lv_gauge_get_line_count_inline");

   function Get_Scale_Angle (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Scale_Angle, "lv_gauge_get_scale_angle_inline");

   function Get_Style (Self : Instance) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_gauge_get_style_inline");

end Lv.Objx.Gauge;
