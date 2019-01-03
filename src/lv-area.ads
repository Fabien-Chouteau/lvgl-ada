pragma Style_Checks (Off);

package LV.Area is

   LV_COORD_MAX : constant := (16383);
   LV_COORD_MIN : constant := (-16384);

   subtype Coord_T is int16_t;

   type Point_T is record
      x : aliased Coord_T;
      y : aliased Coord_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Point_T);

   type Area_T is record
      x1 : aliased Coord_T;
      y1 : aliased Coord_T;
      x2 : aliased Coord_T;
      y2 : aliased Coord_T;
   end record;
   pragma Convention (C_Pass_By_Copy, Area_T);

   procedure Set
     (Area : access Area_T;
      x1   : Coord_T;
      y1   : Coord_T;
      x2   : Coord_T;
      y2   : Coord_T);
   pragma Import (C, set, "lv_area_set");

   procedure Copy (dest : access Area_T; src : access constant Area_T);
   pragma Import (C, copy, "lv_area_copy_inline");

   function Get_Width (area_p : access constant Area_T) return Coord_T;
   pragma Import (C, get_width, "lv_area_get_width_inline");

   function Get_Height (area_p : access constant Area_T) return Coord_T;
   pragma Import (C, get_height, "lv_area_get_height_inline");

   procedure Set_Width (arg1 : access Area_T; arg2 : Coord_T);
   pragma Import (C, set_width, "lv_area_set_width");

   procedure Set_Height (arg1 : access Area_T; arg2 : Coord_T);
   pragma Import (C, set_height, "lv_area_set_height");

   procedure Set_Pos
     (arg1 : access Area_T;
      arg2 : Coord_T;
      arg3 : Coord_T);
   pragma Import (C, set_pos, "lv_area_set_pos");

   function Get_Size (arg1 : access constant Area_T) return uint32_t;
   pragma Import (C, get_size, "lv_area_get_size");

   function Intersect
     (arg1 : access Area_T;
      arg2 : access constant Area_T;
      arg3 : access constant Area_T) return u_Bool;
   pragma Import (C, intersect, "lv_area_intersect");

   procedure Join
     (arg1 : access Area_T;
      arg2 : access constant Area_T;
      arg3 : access constant Area_T);
   pragma Import (C, join, "lv_area_join");

   function Is_Point_On (arg1 : access constant Area_T; arg2 : access constant Point_T) return u_Bool;
   pragma Import (C, is_point_on, "lv_area_is_point_on");

   function Is_On (arg1 : access constant Area_T; arg2 : access constant Area_T) return u_Bool;
   pragma Import (C, is_on, "lv_area_is_on");

   function Is_In (arg1 : access constant Area_T; arg2 : access constant Area_T) return u_Bool;
   pragma Import (C, is_in, "lv_area_is_in");

end LV.Area;
