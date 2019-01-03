pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lv.Style;

package Lv.Objx.Bar is

   subtype Instance is Obj_T;

   subtype Style_T is Uint8_T;

   function Create (Parent : Instance; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_bar_create");

   procedure Set_Value (Self : Instance; Arg2 : Int16_T);
   pragma Import (C, Set_Value, "lv_bar_set_value");

   procedure Set_Value_Anim (Self : Instance; Arg2 : Int16_T; Arg3 : Uint16_T);
   pragma Import (C, Set_Value_Anim, "lv_bar_set_value_anim");

   procedure Set_Range (Self : Instance; Arg2 : Int16_T; Arg3 : Int16_T);
   pragma Import (C, Set_Range, "lv_bar_set_range");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_bar_set_style");

   function Get_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Value, "lv_bar_get_value");

   function Get_Min_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Min_Value, "lv_bar_get_min_value");

   function Get_Max_Value (Self : Instance) return Int16_T;
   pragma Import (C, Get_Max_Value, "lv_bar_get_max_value");

   function Get_Style (Self : Instance; Arg2 : Style_T) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_bar_get_style");

end Lv.Objx.Bar;
