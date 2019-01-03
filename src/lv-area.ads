pragma Style_Checks (Off);

package Lv.Area is

   Lv_Coord_Max : constant := (16383);
   Lv_Coord_Min : constant := (-16384);

   subtype Coord_T is Int16_T;

   type Point_T is record
      X : aliased Coord_T;
      Y : aliased Coord_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Point_T);

   type Area_T is record
      X1 : aliased Coord_T;
      Y1 : aliased Coord_T;
      X2 : aliased Coord_T;
      Y2 : aliased Coord_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Area_T);

   procedure Set
     (Area : access Area_T;
      X1   : Coord_T;
      Y1   : Coord_T;
      X2   : Coord_T;
      Y2   : Coord_T);
   pragma Import (C, Set, "lv_area_set");

   procedure Copy (Dest : access Area_T; Src : access constant Area_T);
   pragma Import (C, Copy, "lv_area_copy_inline");

   function Get_Width (Area_P : access constant Area_T) return Coord_T;
   pragma Import (C, Get_Width, "lv_area_get_width_inline");

   function Get_Height (Area_P : access constant Area_T) return Coord_T;
   pragma Import (C, Get_Height, "lv_area_get_height_inline");

   procedure Set_Width (Arg1 : access Area_T; Arg2 : Coord_T);
   pragma Import (C, Set_Width, "lv_area_set_width");

   procedure Set_Height (Arg1 : access Area_T; Arg2 : Coord_T);
   pragma Import (C, Set_Height, "lv_area_set_height");

   procedure Set_Pos (Arg1 : access Area_T; Arg2 : Coord_T; Arg3 : Coord_T);
   pragma Import (C, Set_Pos, "lv_area_set_pos");

   function Get_Size (Arg1 : access constant Area_T) return Uint32_T;
   pragma Import (C, Get_Size, "lv_area_get_size");

   function Intersect
     (Arg1 : access Area_T;
      Arg2 : access constant Area_T;
      Arg3 : access constant Area_T) return U_Bool;
   pragma Import (C, Intersect, "lv_area_intersect");

   procedure Join
     (Arg1 : access Area_T;
      Arg2 : access constant Area_T;
      Arg3 : access constant Area_T);
   pragma Import (C, Join, "lv_area_join");

   function Is_Point_On
     (Arg1 : access constant Area_T;
      Arg2 : access constant Point_T) return U_Bool;
   pragma Import (C, Is_Point_On, "lv_area_is_point_on");

   function Is_On
     (Arg1 : access constant Area_T;
      Arg2 : access constant Area_T) return U_Bool;
   pragma Import (C, Is_On, "lv_area_is_on");

   function Is_In
     (Arg1 : access constant Area_T;
      Arg2 : access constant Area_T) return U_Bool;
   pragma Import (C, Is_In, "lv_area_is_in");

end Lv.Area;
