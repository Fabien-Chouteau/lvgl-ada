pragma Style_Checks (Off);

with Lv; use Lv;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Lv.Style;
with Lv.Area;

package Lv.Objx.Cont is

   subtype Instance is Obj_T;

   type Layout_T is
     (Layout_Off,
      Layout_Center,
      Layout_Col_L,  --  Column left align
      Layout_Col_M,  --  Column middle align
      Layout_Col_R,  --  Column right align
      Layout_Row_T,  --  Row top align
      Layout_Row_M,  --  Row middle align
      Layout_Row_B,  --  Row bottom align
      Layout_Pretty, --  Put as many object as possible in row and begin a new row
      Layout_Grid    --  Align same-sized object into a grid
      ) with
        Size => 8;

   for Layout_T use
     (Layout_Off    => 0,
      Layout_Center => 1,
      Layout_Col_L  => 2,
      Layout_Col_M  => 3,
      Layout_Col_R  => 4,
      Layout_Row_T  => 5,
      Layout_Row_M  => 6,
      Layout_Row_B  => 7,
      Layout_Pretty => 8,
      Layout_Grid   => 9);

   function Create (Parent : Obj_T; Copy : Obj_T) return Instance;
   pragma Import (C, Create, "lv_cont_create");

   procedure Set_Layout (Self : Instance; Arg2 : Layout_T);
   pragma Import (C, Set_Layout, "lv_cont_set_layout");

   procedure Set_Fit (Self : Instance; Arg2 : U_Bool; Arg3 : U_Bool);
   pragma Import (C, Set_Fit, "lv_cont_set_fit");

   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);
   pragma Import (C, Set_Style, "lv_cont_set_style_inline");

   function Get_Layout (Self : Instance) return Layout_T;
   pragma Import (C, Get_Layout, "lv_cont_get_layout");

   function Get_Hor_Fit (Self : Instance) return U_Bool;
   pragma Import (C, Get_Hor_Fit, "lv_cont_get_hor_fit");

   function Get_Ver_Fit (Self : Instance) return U_Bool;
   pragma Import (C, Get_Ver_Fit, "lv_cont_get_ver_fit");

   function Get_Fit_Width (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Fit_Width, "lv_cont_get_fit_width");

   function Get_Fit_Height (Self : Instance) return Lv.Area.Coord_T;
   pragma Import (C, Get_Fit_Height, "lv_cont_get_fit_height");

   function Get_Style (Self : Instance) return Lv.Style.Style;
   pragma Import (C, Get_Style, "lv_cont_get_style_inline");

end Lv.Objx.Cont;
