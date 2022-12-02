with Lv.Style;

package Lv.Objx.Arc is

   subtype Instance is Obj_T;

   type Style_T is (Style_Main);

   --  Create a arc objects
   --  @param par pointer to an object, it will be the parent of the new arc
   --  @param copy pointer to a arc object, if not NULL then the new object will be copied from it
   --  @return pointer to the created arc
   function Create (Parent : Obj_T; Copy : Instance) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set the start and end angles of an arc. 0 deg: bottom, 90 deg: right etc.
   --  @param arc pointer to an arc object
   --  @param start the start angle [0..360]
   --  @param end the end angle [0..360]
   procedure Set_Angles (Self : Instance; Start : Uint16_T; End_P : Uint16_T);

   -- Set a style of a arc.
   -- @param arc pointer to arc object
   -- @param type which style should be set
   -- @param style pointer to a style
   procedure Set_Style
     (Self   : Instance;
      Type_P : Style_T;
      Style  : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   -- Get the start angle of an arc.
   -- @param arc pointer to an arc object
   -- @return the start angle [0..360]
   function Angle_Start (Self : Instance) return Uint16_T;

   --  Get the end angle of an arc.
   --  @param arc pointer to an arc object
   --  @return the end angle [0..360]
   function Angle_End (Self : Instance) return Uint16_T;

   -- Get style of a arc.
   -- @param arc pointer to arc object
   -- @param type which style should be get
   -- @return style pointer to the style
   function Style
     (Self : Instance;
      Arg2 : Style_T) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_arc_create");
   pragma Import (C, Set_Angles, "lv_arc_set_angles");
   pragma Import (C, Set_Style, "lv_arc_set_style");
   pragma Import (C, Angle_Start, "lv_arc_get_angle_start");
   pragma Import (C, Angle_End, "lv_arc_get_angle_end");
   pragma Import (C, Style, "lv_arc_get_style");

   for Style_T'Size use 8;
   for Style_T use (Style_Main => 0);

end Lv.Objx.Arc;
