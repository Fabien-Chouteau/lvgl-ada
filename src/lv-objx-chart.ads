with Lv.Area;
with Lv.Color;
with Lv.Style;

package Lv.Objx.Chart is

   subtype Instance is Obj_T;

   type Series is private;

   type Chart_Type_T is (Type_Line, Type_Column, Type_Point);

   --  Create a chart background objects
   --  @param par pointer to an object, it will be the parent of the new chart background
   --  @param copy pointer to a chart background object, if not NULL then the new object will be copied from it
   --  @return pointer to the created chart background
   function Create (Par : Obj_T; Copy : Instance) return Instance;

   --------------------------
   -- Add/remove functions --
   --------------------------

   --  Allocate and add a data series to the chart
   --  @param self pointer to a chart object
   --  @param color color of the data series
   --  @return pointer to the allocated data series
   function Add_Series
     (Self  : Instance;
      Color : Lv.Color.Color_T) return Series;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the number of horizontal and vertical division lines
   --  @param self pointer to a graph background object
   --  @param hdiv number of horizontal division lines
   --  @param vdiv number of vertical division lines
   procedure Set_Div_Line_Count
     (Self  : Instance;
      H_Div : Uint8_T;
      V_Div : Uint8_T);

   --  Set the minimal and maximal y values
   --  @param self pointer to a graph background object
   --  @param ymin y minimum value
   --  @param ymax y maximum value
   procedure Set_Range
     (Self  : Instance;
      Y_Min : Lv.Area.Coord_T;
      Y_Max : Lv.Area.Coord_T);

   --  Set a new type for a chart
   --  @param self pointer to a chart object
   --  @param type new type of the chart (from 'lv_chart_type_t' enum)
   procedure Set_Type (Self : Instance; Type_P : Chart_Type_T);

   --  Set the number of points on a data line on a chart
   --  @param self pointer r to chart object
   --  @param point_cnt new number of points on the data lines
   procedure Set_Point_Count (Self : Instance; Point_Cnt : Uint16_T);

   --  Set the opacity of the data series
   --  @param self pointer to a chart object
   --  @param opa opacity of the data series
   procedure Set_Series_Opa (Self : Instance; Opa : Lv.Color.Opa_T);

   --  Set the line width or point radius of the data series
   --  @param self pointer to a chart object
   --  @param width the new width
   procedure Set_Series_Width (Self : Instance; Width : Lv.Area.Coord_T);

   --  Set the dark effect on the bottom of the points or columns
   --  @param self pointer to a chart object
   --  @param dark_eff dark effect level (LV_OPA_TRANSP to turn off)
   procedure Set_Series_Darking (Self : Instance; Dark_Eff : Lv.Color.Opa_T);

   --  Initialize all data points with a value
   --  @param self pointer to chart object
   --  @param ser pointer to a data series on 'chart'
   --  @param y the new value  for all points
   procedure Init_Points
     (Self : Instance;
      Ser  : Series;
      Y    : Lv.Area.Coord_T);

   --  Set the value s of points from an array
   --  @param self pointer to chart object
   --  @param ser pointer to a data series on 'chart'
   --  @param y_array array of 'lv_coord_t' points (with 'points count' elements )
   procedure Set_Points
     (Self    : Instance;
      Ser     : Series;
      Y_Array : access constant Lv.Area.Point_Array);

   --  Shift all data right and set the most right data on a data line
   --  @param self pointer to chart object
   --  @param ser pointer to a data series on 'chart'
   --  @param y the new value of the most right data
   procedure Set_Next (Self : Instance; Ser : Series; Y : Lv.Area.Coord_T);

   --  Set the style of a chart
   --  @param self pointer to a chart object
   --  @param style pointer to a style
   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the type of a chart
   --  @param self pointer to chart object
   --  @return type of the chart (from 'lv_chart_t' enum)
   function Obj_Type (Self : Instance) return Chart_Type_T;

   --  Get the data point number per data line on chart
   --  @param self pointer to chart object
   --  @return point number on each data line
   function Point_Cnt (Self : Instance) return Uint16_T;

   --  Get the opacity of the data series
   --  @param self pointer to chart object
   --  @return the opacity of the data series
   function Series_Opa (Self : Instance) return Lv.Color.Opa_T;

   --  Get the data series width
   --  @param self pointer to chart object
   --  @return the width the data series (lines or points)
   function Series_Width (Self : Instance) return Lv.Area.Coord_T;

   --  Get the dark effect level on the bottom of the points or columns
   --  @param self pointer to chart object
   --  @return dark effect level (LV_OPA_TRANSP to turn off)
   function Series_Darking (Self : Instance) return Lv.Color.Opa_T;

   --  Get the style of an chart object
   --  @param self pointer to an chart object
   --  @return pointer to the chart's style
   function Style (Self : Instance) return Lv.Style.Style;

   ---------------------
   -- Other functions --
   ---------------------

   --  Refresh a chart if its data line has changed
   --  @param self pointer to chart object
   procedure Refresh (Self : Instance);

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_chart_create");
   pragma Import (C, Add_Series, "lv_chart_add_series");
   pragma Import (C, Set_Div_Line_Count, "lv_chart_set_div_line_count");
   pragma Import (C, Set_Range, "lv_chart_set_range");
   pragma Import (C, Set_Type, "lv_chart_set_type");
   pragma Import (C, Set_Point_Count, "lv_chart_set_point_count");
   pragma Import (C, Set_Series_Opa, "lv_chart_set_series_opa");
   pragma Import (C, Set_Series_Width, "lv_chart_set_series_width");
   pragma Import (C, Set_Series_Darking, "lv_chart_set_series_darking");
   pragma Import (C, Init_Points, "lv_chart_init_points");
   pragma Import (C, Set_Points, "lv_chart_set_points");
   pragma Import (C, Set_Next, "lv_chart_set_next");
   pragma Import (C, Set_Style, "lv_chart_set_style_inline");
   pragma Import (C, Obj_Type, "lv_chart_get_type");
   pragma Import (C, Point_Cnt, "lv_chart_get_point_cnt");
   pragma Import (C, Series_Opa, "lv_chart_get_series_opa");
   pragma Import (C, Series_Width, "lv_chart_get_series_width");
   pragma Import (C, Series_Darking, "lv_chart_get_series_darking");
   pragma Import (C, Style, "lv_chart_get_style_inline");
   pragma Import (C, Refresh, "lv_chart_refresh");

   for Chart_Type_T'Size use 8;
   for Chart_Type_T use (Type_Line => 1, Type_Column => 2, Type_Point => 4);

private

   type Series is new System.Address;

end Lv.Objx.Chart;
