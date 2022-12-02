with Lv.Area;
with Lv.Style;

package Lv.Objx.Line is

   subtype Instance is Obj_T;

   --  Create a line objects
   --  @param par pointer to an object, it will be the parent of the new line
   --  @return pointer to the created line
   function Create (Parent : Obj_T; Copy : Obj_T) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set an array of points. The line object will connect these points.
   --  @param self pointer to a line object
   --  @param point_a an array of points. Only the address is saved,
   --  so the array can NOT be a local variable which will be destroyed
   --  @param point_num number of points in 'point_a'
   procedure Set_Points
     (Self      : Instance;
      Point_A   : access constant Lv.Area.Point_Array;
      Point_Num : Uint16_T);

   --  Enable (or disable) the auto-size option. The size of the object will fit to its points.
   --  (set width to x max and height to y max)
   --  @param self pointer to a line object
   --  @param autosize_en true: auto size is enabled, false: auto size is disabled
   procedure Set_Auto_Size (Self : Instance; Autosize : U_Bool);

   --  Enable (or disable) the y coordinate inversion.
   --  If enabled then y will be subtracted from the height of the object,
   --  therefore the y=0 coordinate will be on the bottom.
   --  @param self pointer to a line object
   --  @param yinv_en true: enable the y inversion, false:disable the y inversion
   procedure Set_Y_Invert (Self : Instance; Yinv : U_Bool);

   --  Set the style of a line
   --  @param self pointer to a line object
   --  @param style pointer to a style
   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the auto size attribute
   --  @param self pointer to a line object
   --  @return true: auto size is enabled, false: disabled
   function Auto_Size (Self : Instance) return U_Bool;

   --  Get the y inversion attribute
   --  @param self pointer to a line object
   --  @return true: y inversion is enabled, false: disabled
   function Y_Inv (Self : Instance) return U_Bool;

   --  Get the style of an line object
   --  @param self pointer to an line object
   --  @return pointer to the line's style
   function Style (Self : Instance) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_line_create");
   pragma Import (C, Set_Points, "lv_line_set_points");
   pragma Import (C, Set_Auto_Size, "lv_line_set_auto_size");
   pragma Import (C, Set_Y_Invert, "lv_line_set_y_invert");
   pragma Import (C, Set_Style, "lv_line_set_style_inline");
   pragma Import (C, Auto_Size, "lv_line_get_auto_size");
   pragma Import (C, Y_Inv, "lv_line_get_y_inv");
   pragma Import (C, Style, "lv_line_get_style_inline");

end Lv.Objx.Line;
