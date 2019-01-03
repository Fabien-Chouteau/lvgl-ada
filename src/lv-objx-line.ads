with Interfaces.C.Extensions;
with Interfaces.C; use Interfaces.C;

with Lv.Area;
with Lv.Style;

package Lv.Objx.Line is

   subtype Instance is Obj_T;

   function Create (Parent : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_line_create");

   procedure Set_Points
     (Self : Instance;
      Arg2 : System.Address;
      Arg3 : Uint16_T);
   pragma Import (C, Set_Points, "lv_line_set_points");

   procedure Set_Auto_Size (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Set_Auto_Size, "lv_line_set_auto_size");

   procedure Set_Y_Invert (Self : Instance; Arg2 : U_Bool);
   pragma Import (C, Set_Y_Invert, "lv_line_set_y_invert");

   procedure Set_Style (Self : Instance; Style : access Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_line_set_style_inline");

   procedure Set_Upscale (Self : Instance; Upcale : U_Bool);
   pragma Import (C, Set_Upscale, "lv_line_set_upscale_inline");

   function Get_Auto_Size (Self : Instance) return U_Bool;
   pragma Import (C, Get_Auto_Size, "lv_line_get_auto_size");

   function Get_Y_Inv (Self : Instance) return U_Bool;
   pragma Import (C, Get_Y_Inv, "lv_line_get_y_inv");

   function Get_Style (Self : Instance) return access Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_line_get_style_inline");

   function Get_Upscale (Self : Instance) return U_Bool;
   pragma Import (C, Get_Upscale, "lv_line_get_upscale_inline");

end Lv.Objx.Line;
