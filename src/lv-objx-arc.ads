with Interfaces.C; use Interfaces.C;
with LV.Area;
with LV.Style;

package LV.Objx.Arc is

   subtype Instance is Obj_T;

   subtype style_t is uint8_t;

   function create (Parent : Obj_T; Copy : Instance) return Instance;
   pragma Import (C, create, "lv_arc_create");

   procedure set_angles
     (Self : Instance;
      arg2 : uint16_t;
      arg3 : uint16_t);  -- lv_arc.h:77
   pragma Import (C, set_angles, "lv_arc_set_angles");

   procedure set_style
     (Self : Instance;
      arg2 : style_t;
      arg3 : access LV.Style.Style);  -- lv_arc.h:85
   pragma Import (C, set_style, "lv_arc_set_style");

   function get_angle_start (Self : Instance) return uint16_t;  -- lv_arc.h:96
   pragma Import (C, get_angle_start, "lv_arc_get_angle_start");

   function get_angle_end (Self : Instance) return uint16_t;  -- lv_arc.h:103
   pragma Import (C, get_angle_end, "lv_arc_get_angle_end");

   function get_style (Self : Instance; arg2 : style_t) return access LV.Style.Style;  -- lv_arc.h:111
   pragma Import (C, get_style, "lv_arc_get_style");

--  private
--     type lv_arc_ext_t is record
--        angle_start : aliased lv_area_h.Coord_T;  -- lv_arc.h:37
--        angle_end : aliased lv_area_h.Coord_T;  -- lv_arc.h:38
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_arc_ext_t);  -- lv_arc.h:39

end LV.Objx.Arc;
