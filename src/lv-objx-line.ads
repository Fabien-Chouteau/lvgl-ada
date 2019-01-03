with Interfaces.C.Extensions;
with Interfaces.C; use Interfaces.C;

with LV.Area;
with LV.Style;

package LV.Objx.Line is

   subtype Instance is Obj_T;

   function create (Parent : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, create, "lv_line_create");

   procedure set_points
     (Self : Instance;
      arg2 : System.Address;
      arg3 : uint16_t);  -- lv_line.h:67
   pragma Import (C, set_points, "lv_line_set_points");

   procedure set_auto_size (Self : Instance; arg2 : u_Bool);  -- lv_line.h:75
   pragma Import (C, set_auto_size, "lv_line_set_auto_size");

   procedure set_y_invert (Self : Instance; arg2 : u_Bool);  -- lv_line.h:84
   pragma Import (C, set_y_invert, "lv_line_set_y_invert");

   procedure set_style (Self : Instance; style : access LV.Style.Style);  -- lv_line.h:91
   pragma Import (C, set_style, "lv_line_set_style_inline");

   procedure set_upscale (Self : Instance; upcale : u_Bool);  -- lv_line.h:101
   pragma Import (C, set_upscale, "lv_line_set_upscale_inline");

   function get_auto_size (Self : Instance) return u_Bool;  -- lv_line.h:115
   pragma Import (C, get_auto_size, "lv_line_get_auto_size");

   function get_y_inv (Self : Instance) return u_Bool;  -- lv_line.h:122
   pragma Import (C, get_y_inv, "lv_line_get_y_inv");

   function get_style (Self : Instance) return access LV.Style.Style;  -- lv_line.h:129
   pragma Import (C, get_style, "lv_line_get_style_inline");

   function get_upscale (Self : Instance) return u_Bool;  -- lv_line.h:139
   pragma Import (C, get_upscale, "lv_line_get_upscale_inline");

--  private
--     type lv_lineext_t is record
--        point_array : access constant lv_area_h.Point_T;  -- lv_line.h:38
--        point_num : aliased sys_ustdint_h.uint16_t;  -- lv_line.h:39
--        auto_size : Extensions.Unsigned_1;  -- lv_line.h:40
--        y_inv : Extensions.Unsigned_1;  -- lv_line.h:41
--     end record;
--     pragma Convention (C_Pass_By_Copy, lv_line_ext_t);
--     pragma Pack (lv_line_ext_t);  -- lv_line.h:42

end LV.Objx.Line;
