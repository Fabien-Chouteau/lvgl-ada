package Lv.Area is

   Lv_Coord_Max : constant := (16383);
   Lv_Coord_Min : constant := (-16384);

   subtype Coord_T is Int16_T;

   type Point_T is record
      X : aliased Coord_T;
      Y : aliased Coord_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Point_T);

   type Point_Array is array (Natural range <>) of aliased Lv.Area.Point_T
     with Convention => C;

   type Area_T is record
      X1 : aliased Coord_T;
      Y1 : aliased Coord_T;
      X2 : aliased Coord_T;
      Y2 : aliased Coord_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Area_T);

   --  Initialize an area
   --  @param area_p pointer to an area
   --  @param x1 left coordinate of the area
   --  @param y1 top coordinate of the area
   --  @param x2 right coordinate of the area
   --  @param y2 bottom coordinate of the area
   procedure Set
     (Area : access Area_T;
      X1   : Coord_T;
      Y1   : Coord_T;
      X2   : Coord_T;
      Y2   : Coord_T);

   --  Copy an area
   --  @param dest pointer to the destination area
   --  @param src pointer to the source area
   procedure Copy (Dest : access Area_T; Src : access constant Area_T);

   --  Get the width of an area
   --  @param area_p pointer to an area
   --  @return the width of the area (if x1 == x2 -> width = 1)
   function Get_Width (Area_P : access constant Area_T) return Coord_T;

   --  Get the height of an area
   --  @param area_p pointer to an area
   --  @return the height of the area (if y1 == y2 -> height = 1)
   function Get_Height (Area_P : access constant Area_T) return Coord_T;

   --  Set the width of an area
   --  @param area_p pointer to an area
   --  @param w the new width of the area (w == 1 makes x1 == x2)
   procedure Set_Width (Area_P : access Area_T; W : Coord_T);

   --  Set the height of an area
   --  @param area_p pointer to an area
   --  @param h the new height of the area (h == 1 makes y1 == y2)
   procedure Set_Height (Area_P : access Area_T; H : Coord_T);

   --  Set the position of an area (width and height will be kept)
   --  @param area_p pointer to an area
   --  @param x the new x coordinate of the area
   --  @param y the new y coordinate of the area
   procedure Set_Pos (Area_P : access Area_T; X : Coord_T; Y : Coord_T);

   --  Return with area of an area (x   --  y)
   --  @param area_p pointer to an area
   --  @return size of area
   function Get_Size (Area_P : access constant Area_T) return Uint32_T;

   --  Get the common parts of two areas
   --  @param res_p pointer to an area, the result will be stored her
   --  @param a1_p pointer to the first area
   --  @param a2_p pointer to the second area
   --  @return false: the two area has NO common parts, res_p is invalid
   function Intersect
     (Res : access Area_T;
      A1  : access constant Area_T;
      A2  : access constant Area_T) return U_Bool;

   --  Join two areas into a third which involves the other two
   --  @param res_p pointer to an area, the result will be stored here
   --  @param a1_p pointer to the first area
   --  @param a2_p pointer to the second area
   procedure Join
     (Res : access Area_T;
      A1  : access constant Area_T;
      A2  : access constant Area_T);

   --  Check if a point is on an area
   --  @param a_p pointer to an area
   --  @param p_p pointer to a point
   --  @return false:the point is out of the area
   function Is_Point_On
     (A : access constant Area_T;
      P : access constant Point_T) return U_Bool;

   --  Check if two area has common parts
   --  @param a1_p pointer to an area.
   --  @param a2_p pointer to an other area
   --  @return false: a1_p and a2_p has no common parts
   function Is_On
     (A1 : access constant Area_T;
      A2 : access constant Area_T) return U_Bool;

   --  Check if an area is fully on an other
   --  @param ain_p pointer to an area which could be on aholder_p
   --  @param aholder pointer to an area which could involve ain_p
   --  @return
   function Is_In
     (A_In     : access constant Area_T;
      A_Holder : access constant Area_T) return U_Bool;

   -------------
   -- Imports --
   -------------

   pragma Import (C, Set, "lv_area_set");
   pragma Import (C, Copy, "lv_area_copy_inline");
   pragma Import (C, Get_Width, "lv_area_get_width_inline");
   pragma Import (C, Get_Height, "lv_area_get_height_inline");
   pragma Import (C, Set_Width, "lv_area_set_width");
   pragma Import (C, Set_Height, "lv_area_set_height");
   pragma Import (C, Set_Pos, "lv_area_set_pos");
   pragma Import (C, Get_Size, "lv_area_get_size");
   pragma Import (C, Intersect, "lv_area_intersect");
   pragma Import (C, Join, "lv_area_join");
   pragma Import (C, Is_Point_On, "lv_area_is_point_on");
   pragma Import (C, Is_On, "lv_area_is_on");
   pragma Import (C, Is_In, "lv_area_is_in");

end Lv.Area;
