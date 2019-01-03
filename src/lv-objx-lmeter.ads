with Interfaces.C; use Interfaces.C;
with Lv.Style;

package Lv.Objx.Lmeter is

   subtype Instance is Obj_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_lmeter_create");

   procedure Set_Value (Self : Instance; Arg2 : Int16_T);
   pragma Import (C, Set_Value, "lv_lmeter_set_value");

   procedure Set_Range (Self : Instance; Arg2 : Int16_T; Arg3 : Int16_T);
   pragma Import (C, Set_Range, "lv_lmeter_set_range");

   procedure Set_Scale (Self : Instance; Arg2 : Uint16_T; Arg3 : Uint8_T);
   pragma Import (C, Set_Scale, "lv_lmeter_set_scale");

   procedure Set_Style (Self : Instance; Bg : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_lmeter_set_style_inline");

   function Get_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Value, "lv_lmeter_get_value");

   function Get_Min_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Min_Value, "lv_lmeter_get_min_value");

   function Get_Max_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Max_Value, "lv_lmeter_get_max_value");

   function Get_Line_Count (Self : Instance) return Uint8_T;
   pragma Import (C, Get_Line_Count, "lv_lmeter_get_line_count");

   function Get_Scale_Angle (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Scale_Angle, "lv_lmeter_get_scale_angle");

   function Get_Style (Self : Instance) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_lmeter_get_style_inline");

end Lv.Objx.Lmeter;
