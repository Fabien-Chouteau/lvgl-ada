with Lv; use Lv;
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
      Layout_Grid);  --  Align same-sized object into a grid

   --  Create a container objects
   --  @param par pointer to an object, it will be the parent of the new container
   --  @param copy pointer to a container object, if not NULL then the new object will be copied from it
   --  @return pointer to the created container
   function Create (Parent : Obj_T; Copy : Obj_T) return Instance;

   ----------------------
   -- Setter functions --
   ----------------------

   --  Set a layout on a container
   --  @param self pointer to a container object
   --  @param layout a layout from 'lv_cont_layout_t'
   procedure Set_Layout (Self : Instance; Layout : Layout_T);

   --  Enable the horizontal or vertical fit.
   --  The container size will be set to involve the children horizontally or vertically.
   --  @param self pointer to a container object
   --  @param hor_en true: enable the horizontal fit
   --  @param ver_en true: enable the vertical fit
   procedure Set_Fit (Self : Instance; Hor_En : U_Bool; Ver_En : U_Bool);

   --  Set the style of a container
   --  @param self pointer to a container object
   --  @param style pointer to the new style
   procedure Set_Style (Self : Instance; Style : Lv.Style.Style);

   ----------------------
   -- Getter functions --
   ----------------------

   --  Get the layout of a container
   --  @param self pointer to container object
   --  @return the layout from 'lv_cont_layout_t'
   function Layout (Self : Instance) return Layout_T;

   --  Get horizontal fit enable attribute of a container
   --  @param self pointer to a container object
   --  @return true: horizontal fit is enabled; false: disabled
   function Hor_Fit (Self : Instance) return U_Bool;

   --  Get vertical fit enable attribute of a container
   --  @param self pointer to a container object
   --  @return true: vertical fit is enabled; false: disabled
   function Ver_Fit (Self : Instance) return U_Bool;

   --  Get that width reduced by the horizontal padding. Useful if a layout is used.
   --  @param self pointer to a container object
   --  @return the width which still fits into the container
   function Fit_Width (Self : Instance) return Lv.Area.Coord_T;

   --  Get that height reduced by the vertical padding. Useful if a layout is used.
   --  @param self pointer to a container object
   --  @return the height which still fits into the container
   function Fit_Height (Self : Instance) return Lv.Area.Coord_T;

   --  Get the style of a container
   --  @param self pointer to a container object
   --  @return pointer to the container's style
   function Style (Self : Instance) return Lv.Style.Style;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Create, "lv_cont_create");
   pragma Import (C, Set_Layout, "lv_cont_set_layout");
   pragma Import (C, Set_Fit, "lv_cont_set_fit");
   pragma Import (C, Set_Style, "lv_cont_set_style_inline");
   pragma Import (C, Layout, "lv_cont_get_layout");
   pragma Import (C, Hor_Fit, "lv_cont_get_hor_fit");
   pragma Import (C, Ver_Fit, "lv_cont_get_ver_fit");
   pragma Import (C, Fit_Width, "lv_cont_get_fit_width");
   pragma Import (C, Fit_Height, "lv_cont_get_fit_height");
   pragma Import (C, Style, "lv_cont_get_style_inline");

   for Layout_T'Size use 8;
   for Layout_T use (Layout_Off    => 0,
                     Layout_Center => 1,
                     Layout_Col_L  => 2,
                     Layout_Col_M  => 3,
                     Layout_Col_R  => 4,
                     Layout_Row_T  => 5,
                     Layout_Row_M  => 6,
                     Layout_Row_B  => 7,
                     Layout_Pretty => 8,
                     Layout_Grid   => 9);

end Lv.Objx.Cont;
