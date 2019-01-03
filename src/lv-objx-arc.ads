with Interfaces.C; use Interfaces.C;
with Lv.Area;
with Lv.Style;

package Lv.Objx.Arc is

   subtype Instance is Obj_T;

   subtype Style_T is Uint8_T;

   function Create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, Create, "lv_arc_create");

   procedure Set_Angles (Self : Instance; Arg2 : Uint16_T; Arg3 : Uint16_T);
   pragma Import (C, Set_Angles, "lv_arc_set_angles");

   procedure Set_Style
     (Self : Instance;
      Arg2 : Style_T;
      Arg3 : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_arc_set_style");

   function Get_Angle_Start (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Angle_Start, "lv_arc_get_angle_start");

   function Get_Angle_End (Self : Instance) return Uint16_T;
   pragma Import (C, Get_Angle_End, "lv_arc_get_angle_end");

   function Get_Style
     (Self : Instance;
      Arg2 : Style_T) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_arc_get_style");

end Lv.Objx.Arc;
