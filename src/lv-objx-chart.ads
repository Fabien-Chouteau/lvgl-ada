pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Lv.Area;
with Lv.Color;
with Interfaces.C.Extensions;
with Lv.Style;

package Lv.Objx.Chart is

   subtype Instance is Obj_T;

   type Series is private;

   type Lv_Chart_Type_T is (Type_Line, Type_Column, Type_Point) with
        Size => 8;

   for Lv_Chart_Type_T use (Type_Line => 1, Type_Column => 2, Type_Point => 4);

   function Create (Par : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_chart_create");

   function Add_Series
     (Self : Instance;
      Arg2 : Lv.Color.Color_T) return Series;
   pragma Import (C, Add_Series, "lv_chart_add_series");

   procedure Set_Div_Line_Count
     (Self : Instance;
      Arg2 : Uint8_T;
      Arg3 : Uint8_T);
   pragma Import (C, Set_Div_Line_Count, "lv_chart_set_div_line_count");

   procedure Set_Range
     (Self : Instance;
      Arg2 : Lv.Area.Coord_T;
      Arg3 : Lv.Area.Coord_T);
   pragma Import (C, Set_Range, "lv_chart_set_range");

   procedure Set_Type (Self : Instance; Arg2 : Lv_Chart_Type_T);
   pragma Import (C, Set_Type, "lv_chart_set_type");

   procedure Set_Point_Count (Self : Instance; Arg2 : Uint16_T);
   pragma Import (C, Set_Point_Count, "lv_chart_set_point_count");

   procedure Set_Series_Opa (Self : Instance; Arg2 : Lv.Color.Opa_T);
   pragma Import (C, Set_Series_Opa, "lv_chart_set_series_opa");

   procedure Set_Series_Width (Self : Instance; Arg2 : Lv.Area.Coord_T);
   pragma Import (C, Set_Series_Width, "lv_chart_set_series_width");

   procedure Set_Series_Darking (Self : Instance; Arg2 : Lv.Color.Opa_T);
   pragma Import (C, Set_Series_Darking, "lv_chart_set_series_darking");

   procedure Init_Points
     (Self : Instance;
      Ser  : Series;
      Arg3 : Lv.Area.Coord_T);
   pragma Import (C, Init_Points, "lv_chart_init_points");

   procedure Set_Points
     (Self : Instance;
      Ser  : Series;
      Arg3 : access Lv.Area.Coord_T);
   pragma Import (C, Set_Points, "lv_chart_set_points");

   procedure Set_Next (Self : Instance; Ser : Series; Arg3 : Lv.Area.Coord_T);
   pragma Import (C, Set_Next, "lv_chart_set_next");

   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_chart_set_style_inline");

   function Get_Type (Self : Instance) return Lv_Chart_Type_T;
   pragma Import (C, Get_Type, "lv_chart_get_type");

   function Get_Point_Cnt (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Point_Cnt, "lv_chart_get_point_cnt");

   function Get_Series_Opa (Self : Instance) return Lv.Color.Opa_T;
   pragma Import (C, Get_Series_Opa, "lv_chart_get_series_opa");

   function Get_Series_Width (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Series_Width, "lv_chart_get_series_width");

   function Get_Series_Darking (Self : Instance) return Lv.Color.Opa_T;
   pragma Import (C, Get_Series_Darking, "lv_chart_get_series_darking");

   function Get_Style (Self : Instance) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_chart_get_style_inline");

   procedure Refresh (Self : Instance);
   pragma Import (C, Refresh, "lv_chart_refresh");

private

   type Series is new System.Address;

end Lv.Objx.Chart;
